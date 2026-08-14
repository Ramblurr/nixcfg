#!/usr/bin/env python3
"""Create Pocket ID clients and persist their Caddy-security secrets safely."""

import argparse
import json
import os
import secrets
import stat
import sys
import urllib.error
import urllib.request
import urllib.parse
from pathlib import Path


ENV_FILE = Path("/home/ramblurr/nixcfg-private/.env")
OUTPUT_FILE = Path("/home/ramblurr/nixcfg-private/temp-oidc-secrets.yaml")
GROUP_NAME = "admins"
APPS = (
    ("calibre-gui", "Calibre GUI", "calibre", "calibre-gui-pocket-id"),
    ("files", "Files", "files", "files-pocket-id"),
    ("prowlarr", "Prowlarr", "prowlarr", "prowlarr-pocket-id"),
    ("radarr", "Radarr", "radarr", "radarr-pocket-id"),
    ("sabnzbd", "SABnzbd", "sabnzbd", "sabnzbd-pocket-id"),
    ("sonarr", "Sonarr", "sonarr", "sonarr-pocket-id"),
    ("tube", "Tube Archivist", "tube", "tube-pocket-id"),
)


class ApiError(RuntimeError):
    pass


def validate_dns_hostname(value: str) -> str:
    if not value or value != value.lower() or any(char.isspace() or char == "\\" for char in value):
        raise ApiError("hostname must be lowercase canonical DNS")
    try:
        idna_value = value.encode("idna").decode("ascii")
    except UnicodeError as error:
        raise ApiError("hostname must be valid IDNA") from error
    if not value.isascii() or idna_value != value or len(value) > 253:
        raise ApiError("hostname must be canonical ASCII IDNA")
    labels = value.split(".")
    if any(not label or len(label) > 63 for label in labels):
        raise ApiError("hostname contains an empty or oversized label")
    for label in labels:
        if not (label[0].isalnum() and label[-1].isalnum()):
            raise ApiError("hostname labels must begin and end with alphanumeric characters")
        if any(char not in "abcdefghijklmnopqrstuvwxyz0123456789-" for char in label):
            raise ApiError("hostname labels contain an invalid character")
    return value


def validate_base_url(value: str) -> str:
    if not value or value != value.strip() or "\\" in value or any(char.isspace() for char in value):
        raise ApiError("base URL contains whitespace or a backslash")
    try:
        parsed = urllib.parse.urlsplit(value)
        port = parsed.port
    except ValueError as error:
        raise ApiError("base URL is malformed or has an invalid port") from error
    if (
        parsed.scheme != "https"
        or not parsed.netloc
        or parsed.username is not None
        or parsed.password is not None
        or parsed.query
        or parsed.fragment
        or parsed.path not in ("", "/")
    ):
        raise ApiError("base URL must be an origin-only HTTPS URL")
    hostname = validate_dns_hostname(parsed.hostname or "")
    canonical_netloc = hostname if port is None else f"{hostname}:{port}"
    if parsed.netloc != canonical_netloc:
        raise ApiError("base URL hostname must be canonical and contain no credentials")
    return f"https://{canonical_netloc}"


def load_env_value(name: str) -> str:
    """Read one simple KEY=value entry without evaluating the private env file."""
    try:
        lines = ENV_FILE.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise ApiError(f"cannot read {ENV_FILE}: {error.strerror}") from error

    for line in lines:
        line = line.strip()
        if line.startswith("export "):
            line = line[7:].lstrip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        if key.strip() != name:
            continue
        value = value.strip()
        if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
            value = value[1:-1]
        if value:
            return value
        break
    raise ApiError(f"{name} is absent or empty in {ENV_FILE}")


def api_request(
    base_url: str, api_key: str, method: str, path: str, expected_status: int, payload=None
):
    data = None if payload is None else json.dumps(payload).encode("utf-8")
    request = urllib.request.Request(
        f"{base_url}{path}",
        data=data,
        method=method,
        headers={
            "Accept": "application/json",
            "Content-Type": "application/json",
            "X-API-Key": api_key,
        },
    )
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            status = response.status
            body = response.read()
    except urllib.error.HTTPError as error:
        raise ApiError(f"{method} {path} returned HTTP {error.code}") from error
    except urllib.error.URLError as error:
        raise ApiError(f"{method} {path} failed: {error.reason}") from error

    if status != expected_status:
        raise ApiError(f"{method} {path} returned unexpected HTTP {status}")
    try:
        return json.loads(body)
    except json.JSONDecodeError as error:
        raise ApiError(f"{method} {path} returned invalid JSON") from error


def find_group_id(response: dict) -> str:
    groups = response.get("data")
    if not isinstance(groups, list):
        raise ApiError("group list response has no data array")
    matches = [group.get("id") for group in groups if group.get("name") == GROUP_NAME]
    if len(matches) != 1 or not isinstance(matches[0], str) or not matches[0]:
        raise ApiError(f"expected exactly one {GROUP_NAME!r} group")
    return matches[0]


def target_clients(response: dict) -> dict[str, dict]:
    clients = response.get("data")
    if not isinstance(clients, list):
        raise ApiError("client list response has no data array")
    by_id = {}
    target_ids = {client_id for client_id, _, _, _ in APPS}
    for client in clients:
        if not isinstance(client, dict):
            raise ApiError("client list response contains an invalid client")
        client_id = client.get("id")
        if client_id in target_ids:
            if client_id in by_id:
                raise ApiError(f"target Pocket ID client is duplicated: {client_id}")
            by_id[client_id] = client
    return by_id


def output_keys() -> set[str]:
    expected = {
        f"{client_id}-{kind}"
        for client_id, _, _, _ in APPS
        for kind in ("oidc-client-secret", "caddy-security-signing-key")
    }
    if not OUTPUT_FILE.exists():
        return set()
    try:
        metadata = OUTPUT_FILE.lstat()
        if not stat.S_ISREG(metadata.st_mode) or stat.S_IMODE(metadata.st_mode) != 0o600:
            raise ApiError(f"{OUTPUT_FILE} must be a regular mode-0600 file")
        lines = OUTPUT_FILE.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise ApiError(f"cannot read {OUTPUT_FILE}: {error.strerror}") from error

    keys = set()
    for line in lines:
        key, separator, _value = line.partition(":")
        if not separator or key not in expected or key in keys:
            raise ApiError(f"{OUTPUT_FILE} contains an unexpected or duplicate key")
        keys.add(key)
    return keys


def open_output_file() -> int:
    flags = os.O_WRONLY | os.O_APPEND
    if OUTPUT_FILE.exists():
        flags |= os.O_NOFOLLOW
    else:
        flags |= os.O_CREAT | os.O_EXCL
    try:
        descriptor = os.open(OUTPUT_FILE, flags, 0o600)
        os.fchmod(descriptor, stat.S_IRUSR | stat.S_IWUSR)
        return descriptor
    except OSError as error:
        raise ApiError(f"cannot open {OUTPUT_FILE}: {error.strerror}") from error


def persist_line(descriptor: int, line: str) -> None:
    encoded = line.encode("utf-8")
    while encoded:
        written = os.write(descriptor, encoded)
        if written == 0:
            raise ApiError("cannot persist generated secret")
        encoded = encoded[written:]
    os.fsync(descriptor)


def secret_keys(client_id: str) -> tuple[str, str]:
    return (
        f"{client_id}-oidc-client-secret",
        f"{client_id}-caddy-security-signing-key",
    )


def persist_pair(descriptor: int, client_id: str, client_secret: str) -> None:
    oidc_key, signing_key = secret_keys(client_id)
    persist_line(descriptor, f"{oidc_key}: {json.dumps(client_secret)}\n")
    signing_secret = secrets.token_urlsafe(48)
    persist_line(descriptor, f"{signing_key}: {json.dumps(signing_secret)}\n")


def create_client_payload(client_id: str, name: str, host: str, realm: str) -> dict:
    return {
        "id": client_id,
        "name": name,
        "description": "",
        "callbackURLs": [
            f"https://{host}/auth/oauth2/{realm}/authorization-code-callback"
        ],
        "logoutCallbackURLs": [],
        "isPublic": False,
        "pkceEnabled": True,
        "requiresReauthentication": False,
        "requiresPushedAuthorizationRequests": False,
        "skipConsent": True,
        "credentials": {},
        "launchURL": f"https://{host}",
        "hasLogo": False,
        "hasDarkLogo": False,
        "isGroupRestricted": True,
    }


def verify_existing_client(
    base_url: str, api_key: str, client_id: str, payload: dict, admin_group_id: str
) -> None:
    client = api_request(
        base_url, api_key, "GET", f"/api/oidc/clients/{client_id}", 200
    )
    for field in (
        "id",
        "name",
        "description",
        "callbackURLs",
        "logoutCallbackURLs",
        "isPublic",
        "pkceEnabled",
        "requiresReauthentication",
        "requiresPushedAuthorizationRequests",
        "skipConsent",
        "launchURL",
        "hasLogo",
        "hasDarkLogo",
        "isGroupRestricted",
    ):
        if client.get(field) != payload[field]:
            raise ApiError(f"existing {client_id} does not match the planned {field}")
    groups = client.get("allowedUserGroups")
    if not isinstance(groups, list) or [group.get("id") for group in groups] != [admin_group_id]:
        raise ApiError(f"existing {client_id} does not have the planned group restriction")


def create_client(
    base_url: str, api_key: str, client_id: str, payload: dict, admin_group_id: str
) -> None:
    api_request(base_url, api_key, "POST", "/api/oidc/clients", 201, payload)
    api_request(
        base_url,
        api_key,
        "PUT",
        f"/api/oidc/clients/{client_id}/allowed-user-groups",
        200,
        {"userGroupIds": [admin_group_id]},
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Create Pocket ID clients and write one-time secrets without printing them."
    )
    parser.add_argument("--instance", choices=("home", "work"), default="home")
    parser.add_argument(
        "--base-url", required=True, help="Pocket ID base URL for the selected instance."
    )
    parser.add_argument(
        "--app-domain", required=True, help="Base domain for the protected applications."
    )
    args = parser.parse_args()

    base_url = validate_base_url(args.base_url)
    app_domain = validate_dns_hostname(args.app_domain)
    api_key_name = (
        "HOME_POCKET_ID_API_KEY"
        if args.instance == "home"
        else "WORK_POCKET_ID_API_KEY"
    )

    api_key = load_env_value(api_key_name)
    existing_clients = target_clients(
        api_request(base_url, api_key, "GET", "/api/oidc/clients?limit=100", 200)
    )
    admin_group_id = find_group_id(
        api_request(base_url, api_key, "GET", "/api/user-groups?limit=100", 200)
    )
    keys = output_keys()
    descriptor = open_output_file()
    try:
        for client_id, name, subdomain, realm in APPS:
            host = f"{subdomain}.{app_domain}"
            payload = create_client_payload(client_id, name, host, realm)
            oidc_key, signing_key = secret_keys(client_id)
            has_oidc_key = oidc_key in keys
            has_signing_key = signing_key in keys
            if has_oidc_key != has_signing_key:
                raise ApiError(f"{client_id} has an incomplete persisted secret pair")

            if client_id in existing_clients:
                verify_existing_client(
                    base_url, api_key, client_id, payload, admin_group_id
                )
                if has_oidc_key:
                    continue
                if client_id != "calibre-gui":
                    raise ApiError(
                        f"existing {client_id} has no persisted secret pair; refusing to rotate it"
                    )
            elif has_oidc_key:
                raise ApiError(f"persisted secret pair exists without {client_id} client")
            else:
                create_client(base_url, api_key, client_id, payload, admin_group_id)

            created_secret = api_request(
                base_url,
                api_key,
                "POST",
                f"/api/oidc/clients/{client_id}/secret",
                200,
                {},
            )
            client_secret = created_secret.get("secret")
            if not isinstance(client_secret, str) or not client_secret:
                raise ApiError(f"secret creation for {client_id} returned no secret")
            persist_pair(descriptor, client_id, client_secret)
            keys.update((oidc_key, signing_key))
    finally:
        os.close(descriptor)


if __name__ == "__main__":
    try:
        main()
    except ApiError as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
