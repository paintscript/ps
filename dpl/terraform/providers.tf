terraform {
  required_providers {
    # https://www.terraform.io/docs/configuration/provider-requirements.html

    cloudflare = {
      source = "cloudflare/cloudflare"
    }
  }
}
