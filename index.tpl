<!DOCTYPE html>
<html lang="en">
	<head>
	    <meta charset="utf-8">
	    <title>Terminus DB Management Dashboard</title>
	    <link rel="shortcut icon" type="image/png" href="https://terminusdb.com/t/favicon.png"/>
	    <link rel="stylesheet" type="text/css" href="https://terminusdb.github.io/terminus-dashboard/dist/css/basic.css">
	    <link rel="stylesheet" type="text/css" href="https://terminusdb.github.io/terminus-dashboard/dist/css/theme.css">
	</head>
	<BODY>
	<div class="terminus-client-ui">
		<table class="terminus-client-table-layout" style="width: 100%">
			<tr class="main-table-height">
				<td class='terminus-control-panel terminus-control-panel-table-layout'>
					<table class="terminus-application-controls terminus-control-panel-layout" style='width: 350px;'>
						<tr><th class="terminus-logo">
							 <img src='https://terminusdb.com/t/TerminusDBLogo.png' class='terminus-logo-container'></img></th></tr>
						<tr><th class='terminus-dashboard-btn-group' style='display:flex;'>
							<button class='terminus-dashboard-btn terminus-dashboard-selected' id = "terminus-client-btn">
									Dashboard</button>
							<button class='terminus-dashboard-btn' id="terminus-explorer-btn">
								    Api Explorer</button>
						</th></tr>
						<tr><td class="terminus-application-control terminus-client-body"
								id="terminus-control-panel" ></td></tr>
						<tr><td class="terminus-application-control terminus-expolrer-body terminus-explorer-hide"
										id="terminus-explorer"></td></tr>
					</table>
				</td>
				<td class='terminus-main-window terminus-main-window-full-css' style="width: 100%; vertical-align: top;">
					<div class='terminus-user-messages' id='terminus-user-messages'></div>
                    <div class='terminus-content-layout'>
				   	    <div class='terminus-main-content terminus-print' id='terminus-content-viewer'>
				   	    </div>
                    </div>
				</td>
			</tr>
	</table>
	    <script src="https://terminusdb.github.io/terminus-dashboard/dist/terminus-client.min.js"></script>
	    <script src="https://terminusdb.github.io/terminus-dashboard/dist/terminus-dashboard.min.js"></script>
		<script>
		let TerminusConfig = {
			controls: [
				"server", "db", "api_explorer", "change-server", "import_schema", "create_database",
				"create_document", "get_document", "get_schema", "update_schema", "woql_select"
			],
			plugins: {
				"font-awesome": true,
				gmaps: false,
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

		var FrameConfig = {
			view: {
				label: "View Document",
				load_schema: true, //should we load the document schema or just use the document frame
				editor: false,
				mode: "view",
				viewer: "html",
				hide_disabled_buttons: true,
				rules: [
					{
						pattern: { renderer: "object"},
						output: {
							facet: "page",
							features: ["body", "type", "id"],
						}
					},
					{
						pattern: { renderer: "property"},
						output: {facet: "multiline", features: ["body", "label"]}
					},
					{
						pattern: { renderer: "value"},
						output: {facet: "line", features: ["body"]}
					}
				]
			},
			edit: {
				label: "Edit Document",
				load_schema: true, //should we load the document schema or just use the document frame
				hide_disabled_buttons: true,
				editor: true,
				rules: [
					{
						pattern: { renderer: "object", depth: ">0"},
						output: {
							facet: "page",
							features: ["body", "type", "label", "control"],
							controls: ["add", "delete"]
						}
					},
					{
						pattern: { renderer: "object", depth: 0},
						output: {
							facet: "page",
							features: ["body", "type", "label", "control"],
							controls: ["add", "update", "delete"]
						}
					},
					{
						pattern: { renderer: "property"},
						output: {
							facet: "multiline",
							features: ["body", "label", "control"],
							controls: ["add"]
						}
					},
					{
						pattern: { renderer: "value"},
						output: {
							facet: "line",
							features: ["body", "control"],
		 					controls: ["delete"]
		 				}
					},
					{
						pattern: { renderer: "value", property: "rdfs:comment"},
						output: {
							viewerType: "HTMLStringEditor",
							viewerOptions: { big: true}
						}
					}

				],
				mode: "edit",
				viewer: "html"
			},
			create: {
				label: "Create Document",
				hide_disabled_buttons: true,
				rules: [
					{
						pattern: { renderer: "object", depth: ">0"},
						output: {
							facet: "page",
							features: ["type", "comment", "body", "control"],
							controls: ["add", "delete"]
						}

					},
					{
						pattern: { renderer: "object", depth: 0},
						output: {
							facet: "page",
							features: ["type", "id", "body", "control"],
							controls: ["add", "update", "cancel"]
						}

					},
					{
						pattern: { renderer: "property"},
						output: {
							facet: "multiline",
							features: ["label", "body", "control"],
							controls: ["add"]
						}
					},
					{
						pattern: { renderer: "value"},
						output: {
							facet: "line",
							features: ["body"]
						}
					},
					{
						pattern: { renderer: "value", property: "rdfs:comment"},
						output: {
							viewerType: "HTMLStringEditor",
							viewerOptions: { big: true},
							features: ["body"],
						}
					}
				],
				editor: true,
				mode: "edit",
				viewer: "html"
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
</BODY>
