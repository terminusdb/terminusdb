<!DOCTYPE html>
<html lang="en">
	<head>
	    <meta charset="utf-8">
		<meta name="viewport" content="width=device-width, initial-scale=1">
	    <title>Terminus DB Management Dashboard</title>
		<link rel="shortcut icon" type="image/png" href="https://terminusdb.com/t/favicon.png"/>
		<script src="https://d3js.org/d3.v5.min.js"></script>
		<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.css">
		<link rel="stylesheet" href="https://terminusdb.github.io/terminus-dashboard/dist/css/theme.css">
	 <script src="https://terminusdb.github.io/terminus-dashboard/dist/terminus-dashboard.min.js"></script>
	</head>
	<body>
	<div class="terminus-client-ui">
		<table class="terminus-client-table-layout" style="width: 100%">
			<tr class="main-table-height">
				<td class='terminus-control-panel terminus-control-panel-table-layout'>
					<table class="terminus-application-controls terminus-control-panel-layout" style='width: 350px;'>
						<tr><th class="terminus-logo">
							 <img src='https://terminusdb.github.io/terminus-dashboard/dist/css/img/TerminusDB_Logo_Original.png' class='terminus-logo-container'></img></th></tr>
						<tr><td class="terminus-application-control terminus-client-body"
								id="terminus-control-panel" ></td></tr>
						<tr><td class="terminus-application-control terminus-expolrer-body terminus-explorer-hide"
										id="terminus-explorer"></td></tr>
					</table>
				</td>
				<td class='terminus-main-window terminus-main-window-full-css' style="width: 100%; vertical-align: top;">
					<div class='terminus-user-messages' id='terminus-user-messages'></div>
                    <div class='terminus-content-layout'>
				   	    <div class='terminus-main-content terminus-print' id='terminus-content-viewer'></div>
                    </div>
				</td>
			</tr>
	</table>
	</div>
	<script>

		let TerminusConfig = {
			controls: ["collaborate", "server", "db", "import_schema", "create_database",
						"get_document", "get_schema", "update_schema", "woql_select"],
			css: "theme",
			plugins: {
				"font-awesome": true,
				gmaps: {
					key: "GMAPS_DEV_KEY_HERE",
					loaded: false
				},
				jquery: true,
				jqueryui: false,
				datatables: false,
				quill: true,
				select2: false,
				codemirror: {
					darkMode: false, // true for dark theamed text editor
					loaded: true
				}
			}
		};

		TerminusConfig.location = {server: "~s://~s:~d", key: "~s"};
		
		function loadTerminatorWhenReady(){
			if (typeof TerminusDashboard != "undefined"){
				initTerminator();
			}
			else {
		    	setTimeout(function() { loadTerminatorWhenReady() }, 50);
			}
		}
		
		function initTerminator(){
			var terminator = new TerminusDashboard.TerminusUI(TerminusConfig);
			var pconfig = {};
			pconfig.buttons = {'client'   : document.getElementById("terminus-client-btn"),
							   'explorer' : document.getElementById("terminus-explorer-btn")}
			pconfig.controller 	= document.getElementById("terminus-control-panel");
			pconfig.messages = document.getElementById("terminus-user-messages");
			pconfig.plugins = document.getElementById("terminus-plugin-loader");
			pconfig.explorer = document.getElementById("terminus-explorer");
			pconfig.viewer = document.getElementById("terminus-content-viewer");
			var nlocation = (TerminusConfig && TerminusConfig.location) ? TerminusConfig.location : false;
			terminator.draw(pconfig, nlocation);
		}
		
		loadTerminatorWhenReady();
        </script>
        </body>
</html>
