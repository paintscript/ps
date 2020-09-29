# https://registry.terraform.io/providers/cloudflare/cloudflare/latest/docs
# https://github.com/cloudflare/terraform-provider-cloudflare
# https://api.cloudflare.com
# https://dash.cloudflare.com/ae8cea5eb211c082eb8f3c831f1342e4/paintscript.com/dns

provider "cloudflare" {
  version    = "2.11.0"
  api_token  = var.cf-api-token
  account_id = "ae8cea5eb211c082eb8f3c831f1342e4"
}

variable "z" {
  type    = string
  default = "6ec229ff4b23444b98f8d262c21fcade"
}

resource "cloudflare_record" "gh1" {
  name    = "@"
  type    = "A"
  zone_id = var.z
  value   = "185.199.108.153"
  proxied = true
}

resource "cloudflare_record" "gh2" {
  name    = "@"
  type    = "A"
  zone_id = var.z
  value   = "185.199.109.153"
  proxied = true
}

resource "cloudflare_record" "gh3" {
  name    = "@"
  type    = "A"
  zone_id = var.z
  value   = "185.199.110.153"
  proxied = true
}

resource "cloudflare_record" "gh4" {
  name    = "@"
  type    = "A"
  zone_id = var.z
  value   = "185.199.111.153"
  proxied = true
}
