Shiny.addCustomMessageHandler("validate", function(payload) {
    // schemas is an object containing topic and shared schema
    // still in JSON
    // convert schema items from JSON to object:
    const schemas = payload.schemas
    const instance = payload.instance

    const schemaTopic = JSON.parse(schemas.schemaTopic)
    const schemaShared = JSON.parse(schemas.schemaShared)
    const Ajv2020 = require("ajv/dist/2020");
    const ajv = new Ajv2020({
	schemas: [schemaTopic, schemaShared],
	allErrors: true
    });
    require("ajv-formats")(ajv); // needed to include format constraints (e. g. date-time)
    const validate = ajv.getSchema(schemaTopic.$id)
    const valid = validate(instance)
    const errors = JSON.stringify(validate.errors)
    let validationResult = validate.errors
    if(valid) validationResult = []
    Shiny.setInputValue('validationResult', JSON.stringify(validationResult))

});
