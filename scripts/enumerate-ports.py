import yaml

items = [
    "9003/udp",
    "9100-9200/tcp",
    "30000-30010/tcp",
    "9330-9339/tcp",
    "49863/tcp",
    "52667/tcp",
    "52709/tcp",
    "63098-63100/tcp",
]


def enumerate_ports(items):
    services = {}
    service_id = 0
    for item in items:
        if "-" in item:
            port_range, protocol = item.split("/")
            start_port, end_port = map(int, port_range.split("-"))
            for port in range(start_port, end_port + 1):
                services[f"service{service_id}"] = {
                    "port": port,
                    "protocol": protocol.upper(),
                }
                service_id += 1
        else:
            port, protocol = item.split("/")
            services[f"service{service_id}"] = {
                "port": int(port),
                "protocol": protocol.upper(),
            }
            service_id += 1
    return services


services_dict = enumerate_ports(items)
yaml_output = yaml.dump(services_dict, sort_keys=False)
print(yaml_output)
