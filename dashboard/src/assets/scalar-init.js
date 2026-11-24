(function () {
    var nonceMeta = document.querySelector('meta[name="csp-nonce"]');
    var nonce = nonceMeta ? nonceMeta.content : '';

    var s = document.createElement('script');
    s.id = 'scalar-widget';
    s.nonce = nonce;
    s.src = 'https://cdn.jsdelivr.net/npm/@scalar/api-reference@1.63.0/dist/browser/standalone.js';
    s.onload = function () {
        window.Scalar.createApiReference('#scalar-api', {
            url: 'assets/openapi.json',
            darkMode: false,
            layout: 'modern',
            hideModels: false,
            hideSearch: false,
            showSidebar: true,
            defaultOpenAllTags: false,
            hideClientButton: true,
            showDeveloperTools: 'never',
            slug: 'terminusdb',
            title: 'TerminusDB API',
            telemetry: false,
            theme: 'alternate',
            agent: { disabled: true },
            documentDownloadType: 'none',
        });
    };
    s.onerror = function () {
        document.getElementById('scalar-api').innerHTML =
            '<div style="padding:2rem;text-align:center;font-family:system-ui,sans-serif;color:#94a3b8">' +
            '<h2 style="color:#fff;margin-bottom:0.5rem">Could not load Scalar</h2>' +
            '<p>Check your internet connection.</p></div>';
    };
    document.head.appendChild(s);
})();
