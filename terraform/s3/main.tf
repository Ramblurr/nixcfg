data "sops_file" "secrets" {
  source_file = "../secrets.sops.yaml"
}

locals {
  secrets            = yamldecode(data.sops_file.secrets.raw)
  minio_server       = local.secrets.minio_server
  minio_server_10gbe = local.secrets.minio_server_10gbe
  minio_username     = local.secrets.minio_username
  minio_password     = local.secrets.minio_password
}

provider "minio" {
  minio_server = local.minio_server
  # Note: this user should have the `consoleAdmin` policy attached to it
  minio_user     = local.minio_username
  minio_password = local.minio_password
  minio_ssl      = true
}

resource "random_password" "debord_thanos" {
  length = 32
}
module "debord_thanos_bucket" {
  source           = "./modules/minio-bucket"
  bucket_name      = "debord-thanos"
  is_public        = false
  owner_access_key = "debord-thanos"
  owner_secret_key = "sk-${random_password.debord_thanos.result}"
}

output "debord_thanos_config" {
  sensitive = true
  value = {
    bucket_name      = module.debord_thanos_bucket.bucket_id
    owner_access_key = module.debord_thanos_bucket.owner_access_key
    owner_secret_key = module.debord_thanos_bucket.owner_secret_key
    endpoint         = local.minio_server
  }
}
