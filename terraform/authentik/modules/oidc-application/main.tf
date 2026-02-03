terraform {
  required_providers {
    authentik = {
      source = "goauthentik/authentik"
    }
  }
}

variable "name" {
  type = string
}
variable "slug" {
  type    = string
  default = null
}
variable "authentik_domain" {
  type = string
}
variable "tags" {
  type    = list(string)
  default = []
}
variable "domain" {
  type = string
}
variable "access_token_validity" {
  type    = string
  default = "weeks=8"
}
variable "authorization_flow_id" {
  type = string
}

variable "meta_icon" {
  type    = string
  default = null
}
variable "meta_description" {
  type    = string
  default = null
}
variable "meta_launch_url" {
  type    = string
  default = null
}
variable "group" {
  type = string
}
variable "policy_engine_mode" {
  type    = string
  default = "any"
}

variable "client_id" {
  type = string
}

variable "client_secret" {
  type        = string
  description = "(Optional) Randomly generated if needed and not required "
  sensitive   = true
  default     = null
}

variable "client_type" {
  type    = string
  default = "confidential"
}
variable "authentication_flow_id" {
  type = string
}
variable "redirect_uris" {
  type = list(string)
}

variable "invalidation_flow_id" {
  type = string
}

variable "property_mappings" {
  type    = list(string)
  default = null
}

locals {
  # only need a client secret when its a confidential client type
  client_secret = (
    var.client_type == "confidential"
    ? var.client_secret != null ? var.client_secret : random_password.client_secret.result
    : null
  )
  oidc_url_prefix = "https://${var.authentik_domain}/application/o"
}

resource "random_password" "client_secret" {
  length = 52
}

resource "authentik_provider_oauth2" "main" {
  name                  = var.name
  client_id             = var.client_id
  client_type           = var.client_type
  client_secret         = local.client_secret
  authorization_flow    = var.authorization_flow_id
  authentication_flow   = var.authentication_flow_id
  invalidation_flow     = var.invalidation_flow_id
  allowed_redirect_uris = [for uri in var.redirect_uris : { matching_mode = "strict", url = uri }]
  access_token_validity = var.access_token_validity
  property_mappings     = var.property_mappings
  lifecycle {
    ignore_changes = [
      signing_key
    ]
  }
}

resource "authentik_application" "main" {
  name               = title(var.name)
  slug               = coalesce(var.slug, var.name)
  group              = var.group
  policy_engine_mode = "any"
  meta_launch_url    = coalesce(var.meta_launch_url, "https://${var.domain}")
  meta_icon          = var.meta_icon
  meta_description   = var.meta_description
  protocol_provider  = authentik_provider_oauth2.main.id
}

output "application_id" {
  value = authentik_application.main.uuid
}
output "oauth2_provider_id" {
  value = authentik_provider_oauth2.main.id
}
