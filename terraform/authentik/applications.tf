
locals {
  icon_base = "https://raw.githubusercontent.com/ramblurr/home-ops/refs/heads/main/icons"
  external_proxy_provider_ids = [
    module.calibre-web.proxy_provider_id,
    #module.linkding.proxy_provider_id,
    #module.actual-budget.proxy_provider_id
  ]
  internal_proxy_provider_ids = [
    #module.archivebox.proxy_provider_id,
    module.calibre-web.proxy_provider_id,
    module.calibre.proxy_provider_id,
    module.filebrowser.proxy_provider_id,
    module.sabnzbd.proxy_provider_id,
    module.prowlarr.proxy_provider_id,
    module.sonarr.proxy_provider_id,
    module.radarr.proxy_provider_id,
    module.tubearchivist.proxy_provider_id
  ]
  external_work_proxy_provider_ids = [
  ]
  implicit_authorization_flow        = resource.authentik_flow.provider-authorization-implicit-consent.uuid
  explicit_authorization_flow        = data.authentik_flow.default-provider-authorization-explicit-consent.id
  default_authentication_flow        = data.authentik_flow.default-authentication-flow.id
  default_provider_invalidation_flow = data.authentik_flow.default-provider-invalidation-flow.id
  default_invalidation_flow          = data.authentik_flow.default-invalidation-flow.id

  admin_apps = {
    "grafana"       = module.grafana.application_id
    "calibre"       = module.calibre.application_id
    "filebrowser"   = module.filebrowser.application_id
    "sabnzbd"       = module.sabnzbd.application_id
    "prowlarr"      = module.prowlarr.application_id
    "sonarr"        = module.sonarr.application_id
    "radarr"        = module.radarr.application_id
    "tubearchivist" = module.tubearchivist.application_id
    # "linkding"    = module.linkding.application_id
  }

  household_apps = {
    "home-ocis"      = module.home-ocis-web.application_id
    "audiobookshelf" = module.audiobookshelf.application_id
    #"actual-budget" = module.actual-budget.application_id
  }
}

# module "archivebox" {
#   source                  = "./modules/forward-auth-application"
#   name                    = "archivebox"
#   domain                  = "archive.${var.external_domain}"
#   group                   = "Home"
#   authorization_flow_uuid = local.implicit_authorization_flow
#   invalidation_flow_uuid  = local.default_provider_invalidation_flow
#   meta_icon               = "${local.icon_base}/archivebox.png"
#   skip_path_regex         = <<-EOT
# /add/.*
# /add
# EOT
# }

module "calibre-web" {
  source                  = "./modules/forward-auth-application"
  name                    = "calibre-web"
  domain                  = "books.${var.external_domain}"
  group                   = "Books"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/calibre-web.png"
}

module "calibre" {
  source                  = "./modules/forward-auth-application"
  name                    = "calibre"
  domain                  = "calibre.${var.internal_domain}"
  group                   = "admin"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/calibre.png"
}


module "filebrowser" {
  source = "./modules/forward-auth-application"
  # Keep the existing app/provider identity stable; this backs the
  # NixOS filebrowser-quantum service at files.socozy.casa.
  name                    = "filebrowser"
  domain                  = "files.${var.internal_domain}"
  group                   = "admin"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/filebrowser.png"
}

module "radarr" {
  source                  = "./modules/forward-auth-application"
  name                    = "radarr"
  domain                  = "radarr.${var.internal_domain}"
  group                   = "admin"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/radarr.png"
}

module "sonarr" {
  source                  = "./modules/forward-auth-application"
  name                    = "sonarr"
  domain                  = "sonarr.${var.internal_domain}"
  group                   = "admin"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/sonarr.png"
}

module "prowlarr" {
  source                  = "./modules/forward-auth-application"
  name                    = "prowlarr"
  domain                  = "prowlarr.${var.internal_domain}"
  group                   = "admin"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/prowlarr.png"
}

module "sabnzbd" {
  source                  = "./modules/forward-auth-application"
  name                    = "sabnzbd"
  domain                  = "sabnzbd.${var.internal_domain}"
  group                   = "admin"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/sabnzbd.png"
}

module "tubearchivist" {
  source                  = "./modules/forward-auth-application"
  name                    = "tubearchivist"
  domain                  = "tube.${var.internal_domain}"
  group                   = "Home"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/tubearchivist.png"
  skip_path_regex         = "/api/.*"
}

module "paperless" {
  source                  = "./modules/forward-auth-application"
  name                    = "paperless"
  domain                  = "paperless.${var.internal_domain}"
  group                   = "Home"
  authorization_flow_uuid = local.implicit_authorization_flow
  invalidation_flow_uuid  = local.default_provider_invalidation_flow
  meta_icon               = "${local.icon_base}/paperless.png"
  skip_path_regex         = "/api/.*"
}

#module "linkding" {
#  source                  = "./modules/forward-auth-application"
#  name                    = "linkding"
#  domain                  = "bookmarks.${var.external_domain}"
#  group                   = "Home"
#  authorization_flow_uuid = local.implicit_authorization_flow
#  invalidation_flow_uuid  = local.default_provider_invalidation_flow
#  meta_icon               = "${local.icon_base}/linkding.png"
#  skip_path_regex         = "/(api|feeds)/"
#}

# module "actual-budget" {
#   source                  = "./modules/forward-auth-application"
#   name                    = "actual-budget"
#   domain                  = "budget.${var.external_domain}"
#   group                   = "Home"
#   authorization_flow_uuid = local.implicit_authorization_flow
#   invalidation_flow_uuid  = local.default_provider_invalidation_flow
#   meta_icon               = "${local.icon_base}/actual-budget.png"
# }

module "audiobookshelf" {
  source                 = "./modules/oidc-application"
  name                   = "audiobookshelf"
  client_id              = "audiobookshelf"
  client_secret_special  = false
  domain                 = "audiobookshelf.${var.external_domain}"
  group                  = "Books"
  authorization_flow_id  = local.explicit_authorization_flow
  authentication_flow_id = local.default_authentication_flow
  invalidation_flow_id   = local.default_provider_invalidation_flow
  redirect_uris = [
    "https://audiobookshelf.${var.external_domain}/auth/openid/callback",
    "https://audiobookshelf.${var.external_domain}/auth/openid/mobile-redirect"
  ]
  property_mappings     = data.authentik_property_mapping_provider_scope.oauth2.ids
  access_token_validity = "hours=72"
  authentik_domain      = var.authentik_domain
  meta_icon             = "${local.icon_base}/audiobookshelf.png"
}

module "grafana" {
  source                 = "./modules/oidc-application"
  name                   = "grafana"
  client_id              = "grafana"
  domain                 = "grafana.${var.internal_domain}"
  group                  = "Books"
  authorization_flow_id  = local.explicit_authorization_flow
  authentication_flow_id = local.default_authentication_flow
  invalidation_flow_id   = local.default_provider_invalidation_flow
  redirect_uris          = ["https://grafana.${var.internal_domain}/login/generic_oauth"]
  property_mappings      = data.authentik_property_mapping_provider_scope.oauth2.ids
  access_token_validity  = "hours=4"
  authentik_domain       = var.authentik_domain
  meta_icon              = "${local.icon_base}/grafana.png"
}


#module "paperless" {
#  source                 = "./modules/oidc-application"
#  name                   = "paperless"
#  client_id              = "paperless"
#  domain                 = "paperless.${var.internal_domain}"
#  group                  = "Home"
#  authorization_flow_id  = local.implicit_authorization_flow
#  authentication_flow_id = local.default_authentication_flow
#  redirect_uris          = ["https://paperless.${var.internal_domain}/accounts/oidc/authentik/login/callback/"]
#  property_mappings      = data.authentik_property_mapping_provider_scope.oauth2.ids
#  access_token_validity  = "hours=72"
#  authentik_domain       = var.authentik_domain
#  meta_icon              = "${local.icon_base}/paperless-ngx.png"
#}
