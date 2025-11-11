"""Tests for authentication."""

from zapier_triggers_api.auth import generate_api_key, hash_api_key, verify_api_key


def test_generate_api_key() -> None:
    """Test API key generation."""
    key = generate_api_key()
    assert key.startswith("zap_live_")
    assert len(key) > 60

    test_key = generate_api_key("zap_test_")
    assert test_key.startswith("zap_test_")


def test_hash_and_verify_api_key() -> None:
    """Test API key hashing and verification."""
    key = "test_key_12345"
    hashed = hash_api_key(key)

    assert hashed != key
    assert verify_api_key(key, hashed)
    assert not verify_api_key("wrong_key", hashed)
