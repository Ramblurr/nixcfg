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


async def decide(reader, writer, *, token, gateway, gateway_port, denied):
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
            await reader.readexactly(length)
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
    server = await asyncio.start_server(
        lambda r, w: decide(r, w, token=token, gateway=gateway,
                            gateway_port=args.gateway_port, denied=denied),
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
    asyncio.run(main(parser.parse_args()))
