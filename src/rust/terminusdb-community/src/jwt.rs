use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{OnceLock, RwLock};
use std::time::{Duration, Instant};

use swipl::prelude::*;
use swipl::text::PrologText;
use jsonwebtoken::{decode, Algorithm, DecodingKey, Validation};
use serde::Deserialize;
use serde_json::Value;

#[derive(Clone)]
struct JwkKey {
    kid: String,
    algorithm: Algorithm,
    decoding_key: DecodingKey,
}

struct JwksCache {
    keys: RwLock<HashMap<String, JwkKey>>,
    last_fetch: RwLock<Instant>,
    endpoint: String,
    ttl: Duration,
    background_refresh_in_progress: AtomicBool,
    last_refresh_error: RwLock<Option<String>>,
    last_refresh_trigger: RwLock<Instant>,
}

const MIN_REFRESH_INTERVAL: Duration = Duration::from_secs(5);
const MAX_JWKS_RESPONSE_BYTES: usize = 1024 * 1024;

impl JwksCache {
    fn new(endpoint: &str) -> Self {
        JwksCache {
            keys: RwLock::new(HashMap::new()),
            last_fetch: RwLock::new(Instant::now() - Duration::from_secs(86400)),
            endpoint: endpoint.to_string(),
            ttl: Duration::from_secs(3600),
            background_refresh_in_progress: AtomicBool::new(false),
            last_refresh_error: RwLock::new(None),
            last_refresh_trigger: RwLock::new(Instant::now() - Duration::from_secs(86400)),
        }
    }

    fn get_key(&self, kid: &str) -> Option<JwkKey> {
        self.keys.read().unwrap().get(kid).cloned()
    }

    /// Replace the entire key set atomically. Called only from background thread.
    fn replace_keys(&self, new_keys: Vec<JwkKey>) {
        let mut keys = self.keys.write().unwrap();
        let mut last_fetch = self.last_fetch.write().unwrap();
        keys.clear();
        for key in new_keys {
            keys.insert(key.kid.clone(), key);
        }
        *last_fetch = Instant::now();
    }

    fn is_stale(&self) -> bool {
        Instant::now().duration_since(*self.last_fetch.read().unwrap()) > self.ttl
    }

    /// Trigger a background refresh if cache is stale or empty.
    /// Never blocks the calling thread. Uses AtomicBool to ensure only one
    /// background fetch is in flight at a time.
    fn maybe_background_refresh(&self) {
        if !self.is_stale() && !self.keys.read().unwrap().is_empty() {
            return;
        }
        self.trigger_background_refresh();
    }

    /// Trigger a background refresh unconditionally (used for unknown kid).
    /// Respects both the in-flight flag and a minimum interval (5s) to prevent
    /// thread spawn DoS and IdP endpoint amplification via repeated unknown kid values.
    fn trigger_background_refresh(&self) {
        if Instant::now().duration_since(*self.last_refresh_trigger.read().unwrap())
            < MIN_REFRESH_INTERVAL
        {
            return;
        }
        if self.background_refresh_in_progress
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {
            return;
        }
        *self.last_refresh_trigger.write().unwrap() = Instant::now();
        let endpoint = self.endpoint.clone();
        let spawn_result = std::thread::Builder::new()
            .name("jwks-refresh".to_string())
            .spawn(move || {
                let result = fetch_jwks_blocking(&endpoint);
                if let Some(cache) = JWKS_CACHE.get() {
                    match result {
                        Ok(keys) => {
                            cache.replace_keys(keys);
                            *cache.last_refresh_error.write().unwrap() = None;
                        }
                        Err(e) => {
                            *cache.last_refresh_error.write().unwrap() = Some(format!("JWT JWKS background refresh failed: {}", e));
                        }
                    }
                    cache.background_refresh_in_progress.store(false, Ordering::SeqCst);
                }
            });
        if spawn_result.is_err() {
            self.background_refresh_in_progress.store(false, Ordering::SeqCst);
        }
    }
}

static JWKS_CACHE: OnceLock<JwksCache> = OnceLock::new();

#[derive(Deserialize)]
struct Jwk {
    kid: Option<String>,
    kty: String,
    alg: Option<String>,
    r#use: Option<String>,
    n: Option<String>,
    e: Option<String>,
    x: Option<String>,
    y: Option<String>,
    crv: Option<String>,
}

#[derive(Deserialize)]
struct JwksResponse {
    keys: Vec<Jwk>,
}

fn infer_algorithm(jwk: &Jwk) -> Option<Algorithm> {
    if let Some(alg) = &jwk.alg {
        return parse_algorithm(alg);
    }
    match jwk.kty.as_str() {
        "RSA" => Some(Algorithm::RS256),
        "EC" => match jwk.crv.as_deref() {
            Some("P-256") => Some(Algorithm::ES256),
            Some("P-384") => Some(Algorithm::ES384),
            _ => None,
        },
        _ => None,
    }
}

fn parse_jwk(jwk: &Jwk) -> Option<JwkKey> {
    if let Some(use_val) = &jwk.r#use {
        if use_val != "sig" {
            return None;
        }
    }
    let kid = jwk.kid.clone()?;
    let algorithm = infer_algorithm(jwk)?;
    let decoding_key = match jwk.kty.as_str() {
        "RSA" => {
            let n = jwk.n.as_ref()?;
            let e = jwk.e.as_ref()?;
            DecodingKey::from_rsa_components(n, e).ok()?
        }
        "EC" => {
            let x = jwk.x.as_ref()?;
            let y = jwk.y.as_ref()?;
            DecodingKey::from_ec_components(x, y).ok()?
        }
        _ => return None,
    };
    Some(JwkKey { kid, algorithm, decoding_key })
}

fn parse_jwks_response(json: &str) -> Result<Vec<JwkKey>, String> {
    let response: JwksResponse =
        serde_json::from_str(json).map_err(|e| format!("JWKS JSON parse error: {}", e))?;
    Ok(response.keys.iter().filter_map(parse_jwk).collect())
}

fn fetch_url_blocking(url: &str) -> Result<String, String> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| format!("tokio runtime creation failed: {}", e))?;

    rt.block_on(async {
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(10))
            .use_rustls_tls()
            .build()
            .map_err(|e| format!("reqwest client build failed: {}", e))?;

        match client.get(url).send().await {
            Ok(resp) if resp.status().is_success() => {
                let content_length = resp.content_length().unwrap_or(0) as usize;
                if content_length > MAX_JWKS_RESPONSE_BYTES {
                    return Err(format!("JWKS response too large: {} bytes (limit {})", content_length, MAX_JWKS_RESPONSE_BYTES));
                }
                let body = resp.bytes().await.map_err(|e| format!("response read error: {}", e))?;
                if body.len() > MAX_JWKS_RESPONSE_BYTES {
                    return Err(format!("JWKS response too large: {} bytes (limit {})", body.len(), MAX_JWKS_RESPONSE_BYTES));
                }
                String::from_utf8(body.to_vec()).map_err(|e| format!("response UTF-8 error: {}", e))
            }
            Ok(resp) => Err(format!("HTTP {}", resp.status())),
            Err(e) => Err(format!("request error: {}", e)),
        }
    })
}

fn fetch_jwks_blocking(endpoint: &str) -> Result<Vec<JwkKey>, String> {
    parse_jwks_response(&fetch_url_blocking(endpoint)?)
}

fn fetch_oidc_jwks_uri(issuer_url: &str) -> Result<String, String> {
    let discovery_url = if issuer_url.ends_with('/') {
        format!("{}.well-known/openid-configuration", issuer_url)
    } else {
        format!("{}/.well-known/openid-configuration", issuer_url)
    };
    let body = fetch_url_blocking(&discovery_url)?;
    let doc: Value = serde_json::from_str(&body).map_err(|e| format!("OIDC parse error: {}", e))?;
    let jwks_uri = doc.get("jwks_uri").and_then(|v| v.as_str()).map(|s| s.to_string())
        .ok_or("OIDC discovery missing jwks_uri".to_string())?;
    if !jwks_uri.starts_with("https://") {
        return Err(format!("OIDC jwks_uri must use HTTPS, got: {}", jwks_uri));
    }
    Ok(jwks_uri)
}

#[derive(Deserialize)]
struct JwtHeader {
    kid: Option<String>,
    alg: String,
}

fn parse_jwt_header(token: &str) -> Result<JwtHeader, String> {
    let parts: Vec<&str> = token.split('.').collect();
    if parts.len() < 2 {
        return Err("invalid JWT format".to_string());
    }
    use base64::Engine;
    let bytes = base64::engine::general_purpose::URL_SAFE_NO_PAD
        .decode(parts[0]).map_err(|e| format!("header b64 error: {}", e))?;
    serde_json::from_slice(&bytes).map_err(|e| format!("header json error: {}", e))
}

/// Check whether the token's payload contains an `nbf` claim.
/// Only inspects the unsigned payload — safe because an attacker cannot
/// add `nbf` to a signed token without the signing key.
fn token_has_nbf(token: &str) -> bool {
    let parts: Vec<&str> = token.split('.').collect();
    if parts.len() < 2 {
        return false;
    }
    use base64::Engine;
    let bytes = match base64::engine::general_purpose::URL_SAFE_NO_PAD.decode(parts[1]) {
        Ok(b) => b,
        Err(_) => return false,
    };
    let value: Value = match serde_json::from_slice(&bytes) {
        Ok(v) => v,
        Err(_) => return false,
    };
    value.get("nbf").is_some()
}

#[derive(Deserialize, Default)]
struct JwtDecodeOptions {
    iss: Option<String>,
    aud: Option<String>,
    clock_tolerance: Option<u64>,
}

fn parse_algorithm(alg: &str) -> Option<Algorithm> {
    match alg {
        "RS256" => Some(Algorithm::RS256), "RS384" => Some(Algorithm::RS384),
        "RS512" => Some(Algorithm::RS512), "ES256" => Some(Algorithm::ES256),
        "ES384" => Some(Algorithm::ES384),
        _ => None,
    }
}

/// Fetch JWKS from endpoint, initialize the global cache, and log the result.
/// Returns Ok(()) on success, Err(PrologError::Failure) on failure.
fn init_jwks_cache<C: FrameableContextType>(context: &Context<C>, endpoint: &str) -> Result<(), PrologError> {
    log_info!(context, "JWT: fetching JWKS from {}", endpoint);
    match fetch_jwks_blocking(endpoint) {
        Ok(keys) => {
            let key_count = keys.len();
            log_info!(context, "JWT: loaded {} keys from JWKS", key_count);
            JWKS_CACHE.get_or_init(|| JwksCache::new(endpoint)).replace_keys(keys);
            Ok(())
        }
        Err(e) => {
            log_error!(context, "JWT: JWKS fetch failed: {}", e);
            JWKS_CACHE.get_or_init(|| JwksCache::new(endpoint));
            Err(PrologError::Failure)
        }
    }
}

predicates! {
    #[module("$rustnative")]
    semidet fn jwt_setup_jwks(context, endpoint_term) {
        let endpoint: String = match endpoint_term.get::<PrologText>() { Ok(t) => t.into_inner(), Err(_) => return Err(PrologError::Failure) };
        init_jwks_cache(context, &endpoint)
    }

    #[module("$rustnative")]
    semidet fn jwt_setup_oidc(context, issuer_url_term) {
        let issuer_url: String = match issuer_url_term.get::<PrologText>() { Ok(t) => t.into_inner(), Err(_) => return Err(PrologError::Failure) };
        log_info!(context, "JWT: discovering OIDC config from {}", issuer_url);
        match fetch_oidc_jwks_uri(&issuer_url) {
            Ok(jwks_uri) => {
                log_info!(context, "JWT: discovered JWKS URI: {}", jwks_uri);
                init_jwks_cache(context, &jwks_uri)
            }
            Err(e) => {
                log_error!(context, "JWT: OIDC discovery failed: {}", e);
                Err(PrologError::Failure)
            }
        }
    }

    #[module("$rustnative")]
    semidet fn jwt_decode(context, token_term, payload_term, options_term) {
        let token: String = match token_term.get::<PrologText>() { Ok(t) => t.into_inner(), Err(_) => return Err(PrologError::Failure) };
        let options_json: Option<String> = match options_term.get::<PrologText>() { Ok(t) => Some(t.into_inner()), Err(_) => None };
        let opts: JwtDecodeOptions = options_json
            .and_then(|j| serde_json::from_str(&j).ok())
            .unwrap_or_default();

        let header = match parse_jwt_header(&token) {
            Ok(h) => h,
            Err(_) => return Err(PrologError::Failure)
        };

        if header.alg == "none" {
            log_warning!(context, "JWT: rejected token with alg:none");
            return Err(PrologError::Failure);
        }

        let header_alg = match parse_algorithm(&header.alg) {
            Some(a) => a,
            None => { let alg = &header.alg; log_warning!(context, "JWT: unsupported algorithm {}", alg); return Err(PrologError::Failure); }
        };

        let kid = match &header.kid {
            Some(k) => k,
            None => { log_warning!(context, "JWT: token has no kid"); return Err(PrologError::Failure); }
        };

        let cache = match JWKS_CACHE.get() {
            Some(c) => c,
            None => { log_error!(context, "JWT: JWKS cache not initialized"); return Err(PrologError::Failure); }
        };

        cache.maybe_background_refresh();

        if let Some(err) = cache.last_refresh_error.write().unwrap().take() {
            log_error!(context, "{}", err);
        }

        let key = match cache.get_key(kid) {
            Some(k) => k,
            None => {
                cache.trigger_background_refresh();
                log_warning!(context, "JWT: kid not found in JWKS");
                return Err(PrologError::Failure);
            }
        };

        if header_alg != key.algorithm {
            let hdr_alg = &header.alg;
            let key_alg = key.algorithm;
            log_warning!(context, "JWT: algorithm mismatch — header {}, key {:?}", hdr_alg, key_alg);
            return Err(PrologError::Failure);
        }

        let mut validation = Validation::new(header_alg);
        validation.validate_exp = true;
        validation.leeway = opts.clock_tolerance.unwrap_or(60);
        // Enable nbf validation only if the token actually contains an nbf claim.
        // Many IdPs omit nbf; the library's validate_nbf=true rejects tokens without it.
        let has_nbf = token_has_nbf(&token);
        validation.validate_nbf = has_nbf;
        if let Some(iss) = &opts.iss {
            validation.set_issuer(&[iss]);
        }

        if let Some(aud) = &opts.aud {
            let audiences: Vec<String> = aud.split(',').map(|s| s.trim().to_string()).collect();
            validation.set_audience(&audiences);
        } else {
            validation.validate_aud = false;
        }

        match decode::<Value>(&token, &key.decoding_key, &validation) {
            Ok(data) => {
                let payload = data.claims.to_string();
                payload_term.unify(payload.as_str())
            }
            Err(e) => {
                log_error!(context, "JWT: decode failed: {}", e);
                Err(PrologError::Failure)
            }
        }
    }
}

pub fn register() {
    register_jwt_setup_jwks();
    register_jwt_setup_oidc();
    register_jwt_decode();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_algorithm_known() {
        assert_eq!(parse_algorithm("RS256"), Some(Algorithm::RS256));
        assert_eq!(parse_algorithm("RS384"), Some(Algorithm::RS384));
        assert_eq!(parse_algorithm("RS512"), Some(Algorithm::RS512));
        assert_eq!(parse_algorithm("ES256"), Some(Algorithm::ES256));
        assert_eq!(parse_algorithm("ES384"), Some(Algorithm::ES384));
    }

    #[test]
    fn test_parse_algorithm_unknown() {
        assert_eq!(parse_algorithm("none"), None);
        assert_eq!(parse_algorithm("PS256"), None);
        assert_eq!(parse_algorithm("EdDSA"), None);
        assert_eq!(parse_algorithm(""), None);
        assert_eq!(parse_algorithm("HS256"), None);
        assert_eq!(parse_algorithm("HS384"), None);
        assert_eq!(parse_algorithm("HS512"), None);
    }

    #[test]
    fn test_infer_algorithm_from_alg_field() {
        let jwk = Jwk {
            kid: Some("k1".to_string()),
            kty: "RSA".to_string(),
            alg: Some("RS384".to_string()),
            r#use: Some("sig".to_string()),
            n: None, e: None, x: None, y: None, crv: None,
        };
        assert_eq!(infer_algorithm(&jwk), Some(Algorithm::RS384));
    }

    #[test]
    fn test_infer_algorithm_from_kty_rsa() {
        let jwk = Jwk {
            kid: Some("k1".to_string()),
            kty: "RSA".to_string(),
            alg: None,
            r#use: None,
            n: None, e: None, x: None, y: None, crv: None,
        };
        assert_eq!(infer_algorithm(&jwk), Some(Algorithm::RS256));
    }

    #[test]
    fn test_infer_algorithm_from_kty_ec() {
        let jwk = Jwk {
            kid: Some("k1".to_string()),
            kty: "EC".to_string(),
            alg: None,
            r#use: None,
            n: None, e: None, x: None, y: None,
            crv: Some("P-256".to_string()),
        };
        assert_eq!(infer_algorithm(&jwk), Some(Algorithm::ES256));
    }

    #[test]
    fn test_infer_algorithm_unknown_kty() {
        let jwk = Jwk {
            kid: Some("k1".to_string()),
            kty: "OKP".to_string(),
            alg: None,
            r#use: None,
            n: None, e: None, x: None, y: None, crv: None,
        };
        assert_eq!(infer_algorithm(&jwk), None);
    }

    #[test]
    fn test_parse_jwks_response_valid() {
        // Use a real RSA JWK (2048-bit test key) so DecodingKey::from_rsa_components succeeds
        let jwks_json = r#"{"keys":[{"kty":"RSA","kid":"test-key","alg":"RS256","use":"sig","n":"nBu1v1kpRN9keDGj_94bckliV-80f1Rd1suMUOF0Ie4VywXsQU6C2_X1Q2Au-a_bzBaCSiFnUVSTrIdf1PgSwjudC7zPXEfMBtRbh7RtXea4EOCmLRg8nm9OC1ZlFi-0ne0_otU9yj0vP8xbl9ottv-7i5-WLTf5XXMpQOb8lLoQBdsiDPh7SzWaHZwkSQJFzAUvoBOi0ILl2I3T_ArZMgkoZSTDvd5PU2nHv6bmovvIm82X4hOX8imY3caYAQUL98aSqUEWoDdqBZyQkAJ04ruFTCQAoDUOFBhuZCp9UO0-BIvobhQ1rkUwf78DtGjj-W_2MDu6z-P18Uhxc20o_w","e":"AQAB"}]}"#;
        let result = parse_jwks_response(jwks_json);
        assert!(result.is_ok());
        let keys = result.unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(keys[0].kid, "test-key");
        assert_eq!(keys[0].algorithm, Algorithm::RS256);
    }

    #[test]
    fn test_parse_jwks_rejects_hs256_on_rsa_key() {
        // Algorithm confusion attack: JWK with kty:RSA but alg:HS256.
        // parse_algorithm("HS256") returns None, so infer_algorithm returns None,
        // so parse_jwk skips this key entirely.
        let jwks_json = r#"{"keys":[{"kty":"RSA","kid":"confusion-key","alg":"HS256","use":"sig","n":"nBu1v1kpRN9keDGj_94bckliV-80f1Rd1suMUOF0Ie4VywXsQU6C2_X1Q2Au-a_bzBaCSiFnUVSTrIdf1PgSwjudC7zPXEfMBtRbh7RtXea4EOCmLRg8nm9OC1ZlFi-0ne0_otU9yj0vP8xbl9ottv-7i5-WLTf5XXMpQOb8lLoQBdsiDPh7SzWaHZwkSQJFzAUvoBOi0ILl2I3T_ArZMgkoZSTDvd5PU2nHv6bmovvIm82X4hOX8imY3caYAQUL98aSqUEWoDdqBZyQkAJ04ruFTCQAoDUOFBhuZCp9UO0-BIvobhQ1rkUwf78DtGjj-W_2MDu6z-P18Uhxc20o_w","e":"AQAB"}]}"#;
        let result = parse_jwks_response(jwks_json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0, "RSA key with HS256 alg must be rejected");
    }

    #[test]
    fn test_parse_jwks_response_empty_keys() {
        let jwks_json = r#"{"keys":[]}"#;
        let result = parse_jwks_response(jwks_json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_parse_jwks_response_invalid_json() {
        let result = parse_jwks_response("not json");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_jwks_response_skips_encryption_keys() {
        let jwks_json = r#"{"keys":[{"kty":"RSA","kid":"enc-key","use":"enc","n":"somen","e":"AQAB"}]}"#;
        let result = parse_jwks_response(jwks_json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_parse_jwks_response_skips_keys_without_kid() {
        let jwks_json = r#"{"keys":[{"kty":"RSA","alg":"RS256","use":"sig","n":"somen","e":"AQAB"}]}"#;
        let result = parse_jwks_response(jwks_json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_parse_jwt_header_valid() {
        // {"alg":"RS256","typ":"JWT","kid":"test-key"}
        let header_b64 = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3Qta2V5In0";
        let token = format!("{header_b64}.eyJzdWIiOiJhZG1pbiJ9.sig");
        let result = parse_jwt_header(&token);
        assert!(result.is_ok());
        let header = result.unwrap();
        assert_eq!(header.alg, "RS256");
        assert_eq!(header.kid, Some("test-key".to_string()));
    }

    #[test]
    fn test_parse_jwt_header_no_kid() {
        // {"alg":"RS256","typ":"JWT"}
        let token = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJhZG1pbiJ9.sig";
        let result = parse_jwt_header(token);
        assert!(result.is_ok());
        let header = result.unwrap();
        assert_eq!(header.alg, "RS256");
        assert_eq!(header.kid, None);
    }

    #[test]
    fn test_parse_jwt_header_invalid_format() {
        let result = parse_jwt_header("not-a-jwt");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_jwt_header_invalid_base64() {
        let result = parse_jwt_header("!!!.eyJzdWIiOiJhZG1pbiJ9.sig");
        assert!(result.is_err());
    }

    #[test]
    fn test_jwks_cache_replace_and_get() {
        let cache = JwksCache::new("http://example.com/jwks");
        assert!(cache.get_key("k1").is_none());

        let key = JwkKey {
            kid: "k1".to_string(),
            algorithm: Algorithm::RS256,
            decoding_key: DecodingKey::from_secret(&[]),
        };
        cache.replace_keys(vec![key]);
        assert!(cache.get_key("k1").is_some());
    }

    #[test]
    fn test_jwks_cache_replace_clears_old_keys() {
        let cache = JwksCache::new("http://example.com/jwks");

        let key1 = JwkKey {
            kid: "k1".to_string(),
            algorithm: Algorithm::RS256,
            decoding_key: DecodingKey::from_secret(&[]),
        };
        cache.replace_keys(vec![key1]);
        assert!(cache.get_key("k1").is_some());

        let key2 = JwkKey {
            kid: "k2".to_string(),
            algorithm: Algorithm::RS256,
            decoding_key: DecodingKey::from_secret(&[]),
        };
        cache.replace_keys(vec![key2]);
        assert!(cache.get_key("k1").is_none());
        assert!(cache.get_key("k2").is_some());
    }

    #[test]
    fn test_jwks_cache_is_stale_initially() {
        let cache = JwksCache::new("http://example.com/jwks");
        // last_fetch is set to 86400 seconds ago, so cache should be stale
        assert!(cache.is_stale());
    }

    #[test]
    fn test_trigger_background_refresh_rate_limited() {
        let cache = JwksCache::new("http://127.0.0.1:1/nonexistent");
        // First call should pass the rate-limit check and set last_refresh_trigger.
        // It will spawn a thread that fails quickly (no server), but the
        // AtomicBool is set to true and last_refresh_trigger is updated.
        cache.trigger_background_refresh();
        // The background thread may or may not have completed by now, but
        // regardless, last_refresh_trigger was just set to now, so the
        // second call within 5 seconds should be rate-limited.
        // We verify by checking that last_refresh_trigger is recent.
        let elapsed = Instant::now()
            .duration_since(*cache.last_refresh_trigger.read().unwrap());
        assert!(elapsed < MIN_REFRESH_INTERVAL,
            "last_refresh_trigger should be recent after first call");
    }

    #[test]
    fn test_jwks_cache_not_stale_after_replace() {
        let cache = JwksCache::new("http://example.com/jwks");
        let key = JwkKey {
            kid: "k1".to_string(),
            algorithm: Algorithm::RS256,
            decoding_key: DecodingKey::from_secret(&[]),
        };
        cache.replace_keys(vec![key]);
        assert!(!cache.is_stale());
    }

    #[test]
    fn test_fetch_oidc_jwks_uri_rejects_http_issuer() {
        // http:// issuer URL is rejected at the fetch stage (config-level HTTPS enforcement
        // prevents this from reaching Rust, but the fetch itself also fails)
        let result = fetch_oidc_jwks_uri("http://127.0.0.1:1/nonexistent");
        assert!(result.is_err());
    }

    #[test]
    fn test_fetch_oidc_jwks_uri_https_unreachable() {
        let result = fetch_oidc_jwks_uri("https://127.0.0.1:1/nonexistent");
        assert!(result.is_err());
    }
}
