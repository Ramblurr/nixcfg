"""SmolVM SMOLMEG2 policy. Runs on the trusted host, never in a guest."""
import argparse
import asyncio
import hmac
import ipaddress
from pathlib import Path


def allowed(address, port, gateway, gateway_port, denied):
    if isinstance(address, ipaddress.IPv6Address):
        if address.ipv4_mapped:
            address = address.ipv4_mapped
        elif address.sixtofour or address.teredo:
            return False
    if address == gateway:
        return port == gateway_port
    return (address.is_global and not address.is_multicast
            and port in (80, 443)
            and not any(address in network for network in denied))


def parse_service(value):
    """PLACEHOLDER:PORT=TARGET:PORT. Placeholders must be in 192.0.2.0/24."""
    placeholder, target = value.split("=", 1)
    address, port = placeholder.rsplit(":", 1)
    host, target_port = target.rsplit(":", 1)
    address = ipaddress.IPv4Address(address)
    if address not in ipaddress.ip_network("192.0.2.0/24"):
        raise ValueError(f"placeholder {address} is outside 192.0.2.0/24")
    return (address, int(port)), (host.strip("[]"), int(target_port))


async def pipe(reader, writer):
    try:
        while data := await reader.read(65536):
            writer.write(data)
            await writer.drain()
        if writer.can_write_eof():
            writer.write_eof()
    except (ConnectionError, OSError):
        pass


async def redirect(reader, writer, target, initial):
    """Carry one placeholder flow to its host target. SmolVM does not replay
    the initial bytes after a redirect answer, so they are written first."""
    try:
        async with asyncio.timeout(5):
            upstream_reader, upstream = await asyncio.open_connection(*target)
    except (TimeoutError, OSError):
        writer.write(b"\x01")
        await writer.drain()
        return
    try:
        writer.write(b"\x02")
        await writer.drain()
        upstream.write(initial)
        await upstream.drain()
        await asyncio.gather(pipe(reader, upstream), pipe(upstream_reader, writer))
    finally:
        upstream.close()


async def decide(reader, writer, *, token, gateway, gateway_port, denied, services=None):
    try:
        async with asyncio.timeout(5):
            header = await reader.readexactly(75)
            if header[:8] != b"SMOLMEG2" or not hmac.compare_digest(header[8:40], token):
                return
            family = header[72]
            if family not in (4, 6):
                return
            address = ipaddress.ip_address(await reader.readexactly(4 if family == 4 else 16))
            port = int.from_bytes(header[73:75], "big")
            length = int.from_bytes(await reader.readexactly(2), "big")
            if length > 8192:
                return
            initial = await reader.readexactly(length)
        if isinstance(address, ipaddress.IPv6Address) and address.ipv4_mapped:
            address = address.ipv4_mapped
        target = (services or {}).get((address, port))
        if target:
            await redirect(reader, writer, target, initial)
            return
        permit = allowed(address, port, gateway, gateway_port, denied)
        writer.write(b"\x00" if permit else b"\x01")
        await writer.drain()
    except (TimeoutError, asyncio.IncompleteReadError, ConnectionError, ValueError):
        pass
    finally:
        writer.close()
        try:
            await writer.wait_closed()
        except ConnectionError:
            pass


async def main(args):
    token = bytes.fromhex(Path(args.token_file).read_text().strip())
    if len(token) != 32:
        raise ValueError("Expected a 32-byte interceptor token")
    gateway = ipaddress.ip_address(args.gateway_ip)
    denied = [ipaddress.ip_network(value) for value in args.deny_cidr]
    services = dict(parse_service(value) for value in args.service)
    server = await asyncio.start_server(
        lambda r, w: decide(r, w, token=token, gateway=gateway,
                            gateway_port=args.gateway_port, denied=denied,
                            services=services),
        "127.0.0.1", args.port, limit=16384,
    )
    async with server:
        await server.serve_forever()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--token-file", required=True)
    parser.add_argument("--gateway-ip", required=True)
    parser.add_argument("--gateway-port", type=int, default=8445)
    parser.add_argument("--port", type=int, default=38896)
    parser.add_argument("--deny-cidr", action="append", default=[])
    parser.add_argument("--service", action="append", default=[],
                        help="PLACEHOLDER:PORT=TARGET:PORT, carried by the broker")
    asyncio.run(main(parser.parse_args()))
