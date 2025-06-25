# CLAUDE.md

## Repository Overview

This is a NixOS configuration repository (`nixcfg`) that manages multiple systems using Nix flakes.
It uses a modular approach to configure various hosts including workstations, servers, and virtual machines.

## Code Style Guidelines

- Never use `with lib;` or `with pkgs;`
  Older code uses the `with lib;` pattern alot, but we NEVER DO THIS anymore.

## Common Commands

### Building and Deploying Configurations

```bash
# Build a host configuration without activating
task build host=<hostname>

# Test a configuration (activate temporarily)
task test host=<hostname>
```

Claude should NEVER EVER run a nixos-rebuild switch, a `task switch`, nor any other command that causes a NixOS host to be switched.

### Managing Flake Inputs

```bash
# Update all flake inputs
task update

# Update a specific flake input
task update:input input=<input-name>
```

### Development

```bash
# Check all flake outputs
task check

# Explain why a package is included
task why-depends host=<hostname> pkg=<package-name>
```

## Architecture and Structure

### Host System Organization

The repository manages several types of systems:

1. **Physical Hosts** (`/hosts/`):
   - Each subdirectory represents a NixOS host configuration
   - Hosts can use either stable or unstable NixOS channels
   - Host configurations include hardware settings, networking, and enabled services

2. **Guest VMs** (`/guests/`):
   - MicroVM configurations for containerized services
   - Each subdirectory represents a guest VM configuration
   - Deployed to host systems using `microvm.nix`

### Module System

The configuration is highly modular with reusable NixOS modules in `/modules/`:

- **Desktop**: Hyprland, KDE, fonts, and desktop applications
- **Development**: Language-specific tooling (Clojure, Python, Node.js, K8s)
- **Services**: Server applications (PostgreSQL, Matrix, Docker, etc.)
- **Shell**: Terminal utilities and shell configuration
- **Hardware**: Hardware-specific configurations
- **Networking**: VPN, firewall, and network configuration

### Key Technologies and Patterns

1. **Flakes**: All configurations use Nix flakes for reproducibility
2. **Channels**: Supports both stable (24.11) and unstable NixOS
3. **Secrets Management**:
   - `secrets.sops.yaml` files use SOPS encryption and are decrypted only at runtime on the target host
       - Plaintext values from SOPS never appear in `/nix/store`
   - Files in `secrets/` directories (e.g., `secrets/global.nix`, `hosts/mali/secrets/local.nix`) use git-crypt
      - Git-crypt encrypted files appear encrypted on GitHub but their contents are visible in `/nix/store`
4. **Impermanence**: Some hosts use impermanent root filesystems
5. **Overlays**: Custom package overlays in `/overlays/`, including selective imports from `nixpkgs-mine`
6. **Home Manager**: User environment management integrated with NixOS

### Package Management

- Custom packages are defined in `/pkgs/`
- Overlays in `/overlays/` modify existing packages
- The `nixpkgs-mine-packages.nix` overlay selectively imports packages from a custom nixpkgs fork
- Packages can be referenced directly as `pkgs.<package-name>`

### Important Files

- `flake.nix` - Main flake definition with inputs and outputs
- `nix/hosts.nix` - Host configuration definitions
- `nix/nixos.nix` - NixOS system builder helper functions
- `modules/default.nix` - Module imports and organization
- `config/site.nix` - Site-specific network configuration

### Testing and Deployment Notes

- Use `task test` to temporarily activate a configuration before committing
- MicroVMs are deployed to host systems and accessed via `.svc.socozy.casa` domains
- The repository supports multi-architecture builds (x86_64, aarch64)
- Build artifacts are cached using Attic when available
