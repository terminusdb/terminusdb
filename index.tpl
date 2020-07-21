<!DOCTYPE html>
<html lang="en" class="h-100">
  <!-- include "meta-head.html" -->
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
    <meta name="description" content="TerminusDB is an open source model driven graph database for knowledge graph representation designed specifically for the web-age.">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
    <link rel="stylesheet" type="text/css" media="screen" href="https://unpkg.com/@fonticonpicker/react-fonticonpicker/dist/fonticonpicker.base-theme.react.css" />
    <link rel="stylesheet" type="text/css" media="screen" href="https://unpkg.com/@fonticonpicker/react-fonticonpicker/dist/fonticonpicker.material-theme.react.css" />
    <link rel="shortcut icon" type="image/png" href="img/favicon.png"/>
    <link
      Rel="stylesheet"
      href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
      integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
      crossorigin="anonymous"
    />
    <link rel="stylesheet" href="~s/terminusdb-console-main.css">

    <title>TerminusDB</title>
    <!--<link rel="stylesheet" href="css/main.css?v=1.0">-->
    <!--<link href="https://fonts.googleapis.com/css?family=Lexend+Deca&display=swap" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css?family=Poppins&display=swap" rel="stylesheet">-->

  </head>
  <body class="h-100">
    <noscript>You need to enable JavaScript to run this app.</noscript>
    <div id="root" class="h-100"></div>
    <script>
      ;((key) => {
        window.TERMINUSDB = { user: { username: "admin" }}
        if (key) window.TERMINUSDB.user.password = key
      })("~s")
    </script>
    <!--
      This HTML file is a template.
      If you open it directly in the browser, you will see an empty page.

      You can add webfonts, meta tags, or analytics to this file.
      The build step will place the bundled scripts into the <body> tag.

      To begin the development, run `npm start` or `yarn start`.
      To create a production bundle, use `npm run build` or `yarn build`.

    <script src="js/main.js"></script>

    <script src="js/homepage.js"></script>-->
   <!-- <script async src="https://www.googletagmanager.com/gtag/js?id=UA-151888980-1"></script>-->
  <script src="~s/terminusdb-console.min.js"></script>
  </body>
</html>

<!-- <script src="bundle.js"></script> -->
