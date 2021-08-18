const fs = require("fs");
const request = require("request");

const getComment = (object) => {
  if (object["@subdocument"]) {
    return object["@documentation"]
      ? object["@documentation"]["@comment"] + " It is a subdocument"
      : "Description pending.";
  }
  return object["@documentation"]
    ? object["@documentation"]["@comment"]
    : "Description pending.";
};

const getObjectClass = (object, key) => {
  if (typeof object[key] === "object" && object[key] !== null) {
    return object[key]["@class"];
  }
  return object[key];
};

const getSuperClass = (object) => {
  return object["@inherits"]
    ? "\n\n **Super class:** `" + object["@inherits"] + "`"
    : "";
};

const getProperties = (object) => {
  if (object["@documentation"] && object["@documentation"]["@properties"]) {
    let properties =
      "\n\n **Properties:** \n\n| Property | Range  | Desc | \n| -------- | ------ | ---- |";
    for (const [key, value] of Object.entries(
      object["@documentation"]["@properties"]
    )) {
      properties +=
        "\n| `" +
        key +
        "` | `" +
        getObjectClass(object, key) +
        "` | " +
        value +
        " |";
    }
    return properties;
  }

  return "";
};

const uniq = (list) => {
  var seen = {};
  return list.filter(function (item) {
    return seen.hasOwnProperty(item["@id"])
      ? false
      : (seen[item["@id"]] = true);
  });
};

const urls = [
    "https://raw.githubusercontent.com/terminusdb/terminusdb/main/src/terminus-schema/woql.json",
    "https://raw.githubusercontent.com/terminusdb/terminusdb/main/src/terminus-schema/ref.json",
    "https://raw.githubusercontent.com/terminusdb/terminusdb/main/src/terminus-schema/repository.json",
    "https://raw.githubusercontent.com/terminusdb/terminusdb/main/src/terminus-schema/system_schema.json",
];

//console.log(process.argv[2])
var destination = process.argv.length > 2 ? process.argv[2] : './';

const getJSONAndGenerateMDFile = async () => {
    for (var i in urls){
        let url = urls[i];
        request(urls[i], function (error, response, body) {
            // parse contents of the json file
            let parsedWoqlJSON = JSON.parse(
                "[" + body.toString().replace(/\n\n/g, ",\n\n") + "]"
            );

            // remove non class objects and sort the list according to the order of Alphabets
            parsedWoqlJSON = parsedWoqlJSON.filter((object) => object["@id"]);
            parsedWoqlJSON = parsedWoqlJSON.sort((a, b) =>
                a["@id"] > b["@id"] ? 1 : b["@id"] > a["@id"] ? -1 : 0
            );
            // remove duplicates from the list
            parsedWoqlJSON = uniq(parsedWoqlJSON);

            //   let mdContents =
            //     "# WOQL JSON-LD Reference \n >**On this page:** The JSON-LD definition of the WOQL language. \n\n implement clients for TerminusDB by constructing JSON-LD messages according to the [WOQL syntax schema](http://terminusdb.com/schema/woql) described below.  \n\n ## WOQL classes";
            let mdContents = "";
            // for each object generate document using the template
            parsedWoqlJSON.forEach((object) => {
                if (object["@id"]) {
                    mdContents +=
                        " \n\n ### " +
                        object["@id"] +
                        ' \n\n <p class="tdb-f">' +
                        getComment(object) +
                        "</p>" +
                        "\n\n **Class:** `" +
                        object["@id"] +
                        "`" +
                        getSuperClass(object) +
                        getProperties(object) +
                        "\n\n---";
                }
            });

            // write the conetents into the file
            var f = destination+url.split("/").pop().split(".")[0]+'.md';

            fs.writeFile(f, mdContents, (err) => {
                if (err) {
                    console.error(err);
                    return;
                }
            });
        });
    };
};

getJSONAndGenerateMDFile();
