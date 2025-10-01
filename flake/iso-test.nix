{ inputs, ... }:
{
  perSystem =
    {
      config,
      self',
      inputs',
      pkgs,
      system,
      ...
    }:
    {
      packages.iso-test = pkgs.writeShellApplication {
        name = "iso-test";
        runtimeInputs = with pkgs; [
          qemu-utils
          qemu_kvm
        ];
        text = ''
          disk1=disk1.qcow2
          if [ ! -f $disk1 ]; then
            qemu-img create -f qcow2 $disk1 8G
          fi
          exec qemu-kvm \
            -boot c \
            -cpu host \
            -smp cores=2 \
            -M pc \
            -m 2G \
            -device virtio-balloon \
            -device virtio-rng-pci \
            -device nvme,serial=deadbeef,drive=nvm \
            -device usb-ehci \
            -device usb-storage,drive=usbdisk \
            -device e1000,netdev=wan0,mac=8c:16:45:ce:0f:f4 \
            -netdev user,id=wan0 \
            -device virtio-net-pci,netdev=lan0,mac=e4:1d:2d:bf:df:51 \
            -netdev user,id=lan0 \
            -device virtio-net-pci,netdev=lan1,mac=e4:1d:2d:bf:df:50 \
            -netdev user,id=lan1 \
            -device virtio-net-pci,netdev=testnet0,mac=8c:1d:2d:bf:df:53 \
            -netdev user,id=testnet0,hostfwd=tcp::2222-:22 \
            -drive file=$disk1,format=qcow2,if=none,id=nvm,cache=unsafe,werror=report \
            -drive if=pflash,format=raw,unit=0,readonly=on,file=${pkgs.OVMF.firmware} \
            -drive id=usbdisk,if=none,readonly=on,file="$(echo ${inputs.self.nixosConfigurations.addams-installer.config.system.build.isoImage}/iso/*.iso)" \
            -virtfs local,path=./shared,mount_tag=host0,security_model=none,id=host0
        '';
      };
    };
}
