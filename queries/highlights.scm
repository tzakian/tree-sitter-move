
;; Types
(type_parameters) @punctuation.bracket
(type_parameter) @type
(type_parameter_identifier) @type
(apply_type)  @type
(ref_type)  @type.ref

;; Comments
(line_comment) @comment
(block_comment) @comment

;; Annotations
(annotation) @annotation

;; Constants
(constant name: (constant_identifier)  @constant.name)
(constant exp: (num_literal)  @constant.value)

;; Function definitions
(function_definition name: (function_identifier)  @function)
(native_function_definition name: (function_identifier)  @function)
(usual_spec_function name: (function_identifier)  @function)
(function_parameter name: (variable_identifier)  @variable.parameter)

;; Module definitions
(module_identity address: (module_identifier)  @module.address)
(module_identity module: (module_identifier)  @module.name)

;; Function calls
(call_expression access: (module_access)  @call.function)

;; Macro calls
(macro_call_expression access: (macro_module_access)  @call.macro)

;; Literals 
(num_literal) @literal.number
(bool_literal) @literal.boolean
(hex_string_literal) @literal.hex_string
(byte_string_literal) @literal.byte_string
(address_literal) @literal.address

;; Binders

;; Operators
(binary_operator) @operator
(unary_op) @operator

;; Keywords
"fun" @keyword
"return" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"native" @keyword
"struct" @keyword
"use" @keyword
"public" @keyword
"public(package)" @keyword
"public(friend)" @keyword
"spec" @keyword
"module" @keyword
"abort" @keyword
"pragma" @keyword
"const" @keyword
"let" @keyword
