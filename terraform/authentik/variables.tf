variable "external_domain" {
  type = string
}
variable "home_ocis_name" {
  type = string
}
variable "external_domain_work" {
  type = string
}
variable "internal_domain" {
  type = string
}
variable "authentik_domain" {
  type = string
}

variable "authentik_domain_work" {
  type = string
}

variable "external_kubernetes_ingress_class_name" {
  type    = string
  default = "external"
}
variable "internal_kubernetes_ingress_class_name" {
  type    = string
  default = "internal"
}
