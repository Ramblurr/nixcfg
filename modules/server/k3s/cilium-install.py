#!/usr/bin/env python
# The intention is to install cilium to k3s using the helm chart, but then "unmanage" so we can move the ownership of it to a Flux CD repo.
#
# This script is based off the ansible tasks:
#  https://github.com/onedr0p/cluster-template/blob/8e145b6a9c43f8e9d4add9145558dacb476b2b2c/bootstrap/templates/ansible/playbooks/tasks/cilium.yaml.j2
#  https://github.com/onedr0p/cluster-template/blob/8e145b6a9c43f8e9d4add9145558dacb476b2b2c/bootstrap/templates/ansible/playbooks/tasks/cruft.yaml.j2


import os
import shutil
import sys
import time

from functools import partial
from typing import Dict, Any
from pathlib import Path

import kubernetes

from kubernetes.dynamic.exceptions import NotFoundError
from kubernetes.dynamic import DynamicClient
from kubernetes.dynamic.resource import Resource, ResourceField, ResourceInstance

# import pprint


### CONFIGURATION ###
KUBECONFIG = "@kubeconfig@"
NODENAME = "@nodename@"
# KUBECONFIG = "/etc/rancher/k3s/k3s.yaml"
# NODENAME = "ibnsina"
### MAIN ###

BOOLEANS_TRUE = frozenset(("y", "yes", "on", "1", "true", "t", 1, 1.0, True))
BOOLEANS_FALSE = frozenset(("n", "no", "off", "0", "false", "f", 0, 0.0, False))

MANIFESTS_DIR = "/var/lib/rancher/k3s/server/manifests/"
INSTALL_MARKER = Path(MANIFESTS_DIR) / ".cilium-install-complete"

# pp = pprint.PrettyPrinter(indent=4)


def coerce_bool(v: Any) -> bool:
    normalized = v
    if isinstance(v, str):
        normalized = v.lower().strip()
        if normalized in BOOLEANS_TRUE:
            return True
        elif normalized in BOOLEANS_FALSE:
            return False
        raise ValueError(f"Invalid boolean value: {v}")


def status_condition(condition: Dict, resource: ResourceInstance) -> bool:
    if not resource.status or not resource.status.conditions:
        return False
    matches = [x for x in resource.status.conditions if x.type == condition["type"]]
    if not matches:
        return False
    # There should never be more than one condition of a specific type
    match: ResourceField = matches[0]
    if match.status == "Unknown":
        if match.status == condition["status"]:
            if "reason" not in condition:
                return True
            if condition["reason"]:
                return match.reason == condition["reason"]
        return False
    status = True if match.status == "True" else False
    if status == coerce_bool(condition["status"]):
        if condition.get("reason"):
            return match.reason == condition["reason"]
        return True
    return False


def waiter(client, namespace, kind, resource_name, condition, timeout=360):
    resource_group_version = client.resources.get(kind=kind, api_version="v1")
    predicate = partial(status_condition, condition)

    start_time = time.time()
    while True:
        elapsed = time.time() - start_time
        try:
            resource = resource_group_version.get(
                name=resource_name, namespace=namespace
            )
            if predicate(resource):
                return resource, elapsed
        except NotFoundError:
            pass

        if elapsed > timeout:
            print("Timeout reached")
            return None, elapsed

        time.sleep(1)


def fetch_resource_type(client, namespace, kind) -> Resource:
    try:
        api_resource = client.resources.get(kind=kind, api_version="v1")
        return api_resource
    except NotFoundError:
        return None


def info(client, namespace, kind, resource_name) -> ResourceInstance:
    try:
        resource_group_version = client.resources.get(kind=kind, api_version="v1")
        resource = resource_group_version.get(name=resource_name, namespace=namespace)
        return resource
    except NotFoundError:
        return None


def wait_for_cilium_installed(client):
    cilium_helmchart = info(client, "kube-system", "HelmChart", "cilium")
    if not cilium_helmchart:
        print("Cilium helm chart not found. Aborting early.")
        return True

    job, elapsed = waiter(
        client,
        "kube-system",
        "Job",
        "helm-install-cilium",
        {"type": "Complete", "status": "True"},
    )

    if not job:
        print("Job/helm-install-cilium not found (ns=kube-system)")
        return False
    return True


def unmanage_cilium(client):
    helmchart_type = fetch_resource_type(client, "kube-system", "HelmChart")
    if not helmchart_type:
        print("HelmChart resource type not found")
        return False

    patch = [
        {
            "op": "add",
            "path": "/metadata/annotations/helmcharts.helm.cattle.io~1unmanaged",
            "value": "true",
        }
    ]
    helmchart_type.patch(
        body=patch,
        name="cilium",
        namespace="kube-system",
        content_type="application/json-patch+json",
    )

    sleep(5)

    cilium_helmchart = fetch(client, "kube-system", "HelmChart", "cilium")
    if not cilium_helmchart:
        print("HelmChart/cilium not found (ns=kube-system)")
        return False
    helmchart_type.patch(
        body=patch,
        name="cilium",
        namespace="kube-system",
        content_type="application/json-patch+json",
    )

    helmchart_type.delete(name="cilium", namespace="kube-system")

    patch = [{"op": "replace", "path": "/metadata/finalizers", "value": []}]
    helmchart_type.patch(
        body=patch,
        name="cilium",
        namespace="kube-system",
        content_type="application/json-patch+json",
    )
    return True


def delete_files(file_names, directory):
    for file_name in file_names:
        file_path = os.path.join(directory, file_name)
        if os.path.exists(file_path):
            print(f"Deleting manifest {file_path}")
            os.remove(file_path)


def cleanup_custom_manifests(client, custom_manifests, directory):
    delete_files(custom_manifests, directory)
    addons = fetch_resource_type(client, "kube-system", "Addon")
    for addon in addons.get().items:
        if addon.metadata.name.startswith("custom-"):
            addons.delete(name=addon.metadata.name, namespace="kube-system")
            print(f"Deleting Addon/{addon.metadata.name} (ns=kube-system) ")


def wait_for_node(client, nodename, timeout=360):
    condition = {"type": "Ready", "status": "True"}
    node, elapsed = waiter(client, "kube-system", "Node", nodename, condition, timeout)
    if not node:
        print(f"Node/{NODENAME} not found")
        return None
    return node


def check_cilium_exists(client):
    ds = fetch_resource_type(client, "kube-system", "DaemonSet")
    cilium = ds.get(name="cilium", namespace="kube-system")
    if not cilium:
        print("DaemonSet/cilium not found (ns=kube-system)")
        return False
    return True


def install_cilium(client):
    helmchart_file = "/etc/rancher/custom/custom-cilium-helmchart.yaml"
    # error out if file doesn't exist
    if not os.path.exists(helmchart_file):
        print(f"File {helmchart_file} does not exist")
        sys.exit(1)
    shutil.copy(helmchart_file, "/var/lib/rancher/k3s/server/manifests/")
    # Give k3s some time to start installing the helm chart
    time.sleep(5)


if __name__ == "__main__":
    if INSTALL_MARKER.exists():
        print(f"Install marker detected at {INSTALL_MARKER}. Exiting cleanly.")
        sys.exit(0)

    # Give k3s some time to start up
    time.sleep(10)

    kubernetes.config.load_kube_config(KUBECONFIG)
    client = DynamicClient(kubernetes.client.ApiClient())

    ok = wait_for_node(client, NODENAME, timeout=600)
    if not ok:
        print(f"Node/{NODENAME} did not become ready after 10 minutes. Aborting.")
        sys.exit(1)

    cilium_exists = check_cilium_exists(client)
    if not cilium_exists:
        print("Cilium does not appear to exist, installing the HelmChart")
        install_cilium(client)
    else:
        print(
            "Cilium appears to already be installed. Continuing to ensure it is is unmanaged."
        )

    ok = wait_for_cilium_installed(client)
    if ok:
        ok = unmanage_cilium(client)
        if not ok:
            print("Could not unmanage cilium successfully")
            sys.exit(1)
    else:
        if cilium_exists:
            print("Cilium is running and has already been unmanaged")
        print("Cilium did not install successfully")
        sys.exit(1)

    cleanup_custom_manifests(client, ["custom-cilium-helmchart.yaml"], MANIFESTS_DIR)
    INSTALL_MARKER.touch()
