{
	inputs = {
		hyprland.url = "github:hyprwm/Hyprland";
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		nur.url = "github:nix-community/NUR";
    impermanence.url = "github:nix-community/impermanence";
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";

		home-manager = {
			url = "github:nix-community/home-manager";
			inputs.nixpkgs.follows = "nixpkgs";
		};
	};

  outputs = inputs@{ alejandra, self, nixpkgs, hyprland, home-manager, nur, ... }: {
	formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    nixosConfigurations =  {
      quine = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          nur.nixosModules.nur
            home-manager.nixosModules.home-manager
            hyprland.nixosModules.default
            { environment.systemPackages = [alejandra.defaultPackage.${system}]; }
        { programs.hyprland.enable = true; }
        ./configuration.nix
        ];
      };
    };
  };
}
