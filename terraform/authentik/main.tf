provider "authentik" {}

resource "authentik_service_connection_kubernetes" "local" {
  name       = "local"
  local      = true
  verify_ssl = false
}

data "sops_file" "secrets" {
  source_file = "../secrets.sops.yaml"
}

locals {
  secrets               = yamldecode(data.sops_file.secrets.raw)
  authentik_domain_work = "auth.${var.external_domain_work}"
}

resource "authentik_brand" "work" {
  domain           = local.authentik_domain_work
  default          = false
  branding_title   = "Outskirts Labs"
  branding_logo    = "https://outskirtslabs.com/outskirts-title3-s.40e13d72.png"
  branding_favicon = "https://outskirtslabs.com/favicon-192x192.cb3b29d2.png"
}
