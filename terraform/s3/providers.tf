terraform {
  required_providers {
    minio = {
      source  = "aminueza/minio"
      version = ">= 3.5.3"
    }
    sops = {
      source  = "carlpett/sops"
      version = "1.2.0"
    }
  }
}
