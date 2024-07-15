"""Modules related to AI agents used in MSTICpy."""

from azure.identity import DefaultAzureCredential, get_bearer_token_provider

token_provider = get_bearer_token_provider(
    DefaultAzureCredential(), "https://cognitiveservices.azure.com/.default"
)

config = {
    "model": "gpt-4o",
    "api_type": "azure",
    "api_version": "2024-02-15-preview",
    "base_url": "https://msticpy-intern-project.openai.azure.com/",
    "azure_ad_token_provider": token_provider,
}
