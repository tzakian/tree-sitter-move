const PRECEDENCE = {
  assign: 1,
  implies: 2, // ==>
  or: 3, // ||
  and: 4, // &&
  eq: 5, // ==
  neq: 5, // !=
  lt: 5, // <
  gt: 5, // >
  le: 5, // <=
  ge: 5, // >=
  range: 6, // ..
  bitor: 7, // |
  xor: 8, // ^
  bitand: 9, // &
  shl: 10, // <<
  shr: 10, // >>
  add: 11, // +
  sub: 11, // -
  mul: 12, // *
  div: 12, // /
  mod: 12, // %,
  unary: 13,
  field: 14,
  call: 15,
}

module.exports = grammar({
  name: 'move',
  extras: $ => [/\s/, $.line_comment, $.block_comment],
  word: $ => $.identifier,
  supertypes: $ => [$._spec_block_target],
  conflicts: $ => [
    [$._struct_identifier, $._variable_identifier, $._function_identifier],
    [$.function_type_parameters],
    [$.name_expression, $.call_expression, $.pack_expression],
  ],

  rules: {
    source_file: $ => repeat($.module_definition),

    // parse use declarations
    use_declaration: $ => seq('use', choice($.use_module, $.use_module_member, $.use_module_members), ';'),
    use_module: $ => seq($.module_identity, optional(seq('as', field('alias', $._module_identifier)))),
    use_module_member: $ => seq($.module_identity, '::', field('use_member', $.use_member)),
    use_module_members: $ => seq($.module_identity, '::', '{', sepBy1(',', field('use_member', $.use_member)), '}'),
    use_member: $ => seq(
      field('member', $.identifier),
      optional(seq('as', field('alias', $.identifier)))
    ),

    // parse top-level decl modifiers
    friend_declaration: $ => seq('friend', $.module_access, ';'),
    visibility_modifier: $ => choice('public', 'public(package)', 'public(friend)'),
    ability: $ => choice(
        'copy',
        'drop',
        'store',
        'key',
    ),

    module_definition: $ => {
      return seq(
        optional(repeat($.annotation)),
        'module',
        field('module_identity', $.module_identity),
        field('module_body', $.module_body),
      );
    },
    module_body: $ => {
      return seq(
        '{',
        repeat(
            seq(optional(repeat($.annotation)), choice(
              $.use_declaration,
              $.friend_declaration,
              $.constant,
              $._function_item,
              $._struct_item,
              $.spec_block,
        ))),
        '}'
      );
    },

    // Annotations
    annotation: $ => seq(
        "#[",
            sepBy1(",", $.annotation_item),
        "]"
    ),

    annotation_item: $ => choice(
        field("name", $.identifier),
        seq(
            field("name", $.identifier),
            "=",
            field("value", $._literal_value)
        ),
        seq(
            field("name", $.identifier),
            "(",
                sepBy1(",", choice($._literal_value, $.identifier)),
            ")"
        ),
        seq(
            field("name", $.identifier),
            "(",
                sepBy1(",", seq($.identifier, '=', choice($.identifier, $._literal_value))),
            ")"
        ),
    ),

    // Constants
    constant: $ => seq(
        'const',
        field('name', alias($.identifier, $.constant_identifier)),
        ':', 
        field('type', $._type), 
        '=', field('expr', $._expression), 
        ";"
    ),

    // Struct definitions
    _struct_item: $ => choice(
      $.native_struct_definition,
      $.struct_definition,
    ),
    native_struct_definition: $ => seq(
      'native',
      $._struct_signature,
      ';'
    ),
    struct_definition: $ => seq(
      $._struct_signature,
      field('struct_fields', $.struct_def_fields),
    ),
    struct_def_fields: $ => seq(
      '{',
      sepBy(',', $.field_annotation),
      '}'
    ),
    field_annotation: $ => seq(
      field('field', $._field_identifier),
      ':',
      field('type', $._type),
    ),
    ability_decls: $ => seq(
        'has',
        sepBy(',', $.ability),
    ),

    _struct_signature: $ => seq(
      'struct',
      field('name', $._struct_identifier),
      optional(field('type_parameters', $.type_parameters)),
      optional(field('ability_declarations', $.ability_decls)),
    ),

    // Function definitions
    _function_item: $ => choice(
      $.native_function_definition,
      $.function_definition,
    ),
    native_function_definition: $ => seq(
      optional($.visibility_modifier),
      'native',
      $._function_signature,
      ';'
    ),
    function_definition: $ => seq(
      $._function_signature,
      field('body', $.block)
    ),
    _function_signature: $ => seq(
      optional('entry'),
      optional($.visibility_modifier),
      optional('entry'),
      'fun',
      field('name', $._function_identifier),
      optional(field('type_parameters', $.type_parameters)),
      field('parameters', $.function_parameters),
      optional(seq(':', field('return_type', $._type))),
    ),
    function_parameters: $ => seq(
      '(',
      sepBy(',', $.function_parameter),
      ')',
    ),

    // Spec block start
    spec_block: $ => seq(
      'spec',
      choice(
        seq(optional(field('target', $._spec_block_target)), field('body', $.spec_body)),
        $._spec_function,
      )
    ),
    _spec_block_target: $ => choice(
      $.identifier,
      alias('module', $.spec_block_target_module),
      $.spec_block_target_schema,
    ),
    spec_block_target_fun: $ => seq('fun', $._function_identifier),
    spec_block_target_struct: $ => seq('struct', $._struct_identifier),
    spec_block_target_schema: $ => seq(
      'schema',
      field('name', $._struct_identifier),
      optional(field('type_parameters', $.type_parameters)),
    ),
    spec_body: $ => seq(
      '{',
      repeat($.use_declaration),
      repeat($._spec_block_memeber),
      '}'
    ),
    _spec_block_memeber: $ => choice(
      $.spec_invariant,
      $._spec_function,
      $.spec_condition,
      $.spec_include,
      $.spec_apply,
      $.spec_pragma,
      $.spec_variable,
      $.spec_let,
    ),
    spec_let: $ => seq(
      'let',
      optional('post'),
      field('name', $.identifier),
      '=',
      field('def', $._expression),
      ';'
    ),
    spec_condition: $ => choice(
      $._spec_condition,
      $._spec_abort_if,
      $._spec_abort_with_or_modifies,
    ),
    _spec_condition_kind: $ => choice(
      'assert',
      'assume',
      'decreases',
      'ensures',
      'succeeds_if',
    ),
    _spec_condition: $ => seq(
      choice(
        field('kind', alias($._spec_condition_kind, $.condition_kind)),
        seq(
          field('kind', alias('requires', $.condition_kind)),
          optional('module'),
        )
      ),
      optional(field('condition_properties', $.condition_properties)),
      field('expr', $._expression),
      ';'
    ),
    _spec_abort_if: $ => seq(
      field('kind', alias('aborts_if', $.condition_kind)),
      optional(field('condition_properties', $.condition_properties)),
      field('expr', $._expression),
      optional(seq('with', field('additional_exp', $._expression))),
      ';'
    ),
    _spec_abort_with_or_modifies: $ => seq(
      field('kind', alias(choice(
        'aborts_with',
        'modifies'
      ), $.condition_kind)),
      optional(field('condition_properties', $.condition_properties)),
      sepBy1(',', field('additional_exp', $._expression)),
      ';'
    ),

    spec_invariant: $ => seq(
      field('kind', alias('invariant', $.condition_kind)),
      optional(field('modifier', alias(choice('update', 'pack', 'unpack', 'module'), $.invariant_modifier))),
      optional(field('condition_properties', $.condition_properties)),
      field('expr', $._expression),
      ';'
    ),
    condition_properties: $ => seq('[', sepBy(',', $.spec_property), ']'),
    spec_include: $ => seq('include', $._expression, ';'),

    spec_apply: $ => seq(
      'apply',
      field('expr', $._expression),
      'to',
      sepBy1(',', $.spec_apply_pattern),
      optional(seq('except', sepBy1(',', $.spec_apply_pattern))),
      ';'
    ),
    spec_apply_pattern: $ => seq(
      optional(choice('public', 'internal')),
      field('name_pattern', $.spec_apply_name_pattern),
      optional(field('type_parameters', $.type_parameters)),
    ),
    spec_apply_name_pattern: $ => /[0-9a-zA-Z_*]+/,

    spec_pragma: $ => seq(
      'pragma',
      sepBy(',', $.spec_property),
      ';'
    ),
    spec_property: $ => seq($.identifier, optional(seq('=', $._literal_value))),

    spec_variable: $ => seq(
      optional(choice('global', 'local')),
      field('name', $.identifier),
      optional(field('type_parameters', $.type_parameters)),
      ':',
      field('type', $._type),
      ';'
    ),

    _spec_function: $ => choice(
      $.native_spec_function,
      $.usual_spec_function,
      $.uninterpreted_spec_function,
    ),

    uninterpreted_spec_function: $ => seq('fun', $._spec_function_signature, ';'),
    native_spec_function: $ => seq('native', 'fun', $._spec_function_signature, ';'),
    usual_spec_function: $ => seq(
      'fun',
      $._spec_function_signature,
      field('body', $.block)
    ),
    _spec_function_signature: $ => seq(
      field('name', $._function_identifier),
      optional(field('type_parameters', $.type_parameters)),
      field('parameters', $.function_parameters),
      seq(':', field('return_type', $._type)),
    ),

    // Spec block end


    // move type grammar
    _type: $ => choice(
      $.apply_type,
      $.ref_type,
      $.tuple_type,
      $.function_type,
    ),
    apply_type: $ => seq(
      $.module_access,
      optional(field('type_arguments', $.type_arguments)),
    ),
    ref_type: $ => seq(
      field('mutable', choice('&', '&mut')),
      $._type
    ),
    tuple_type: $ => seq('(', sepBy1(',', $._type), ')'),
    primitive_type: $ => choice(
      'u8',
      'u16',
      'u32',
      'u64',
      'u128',
      'u256',
      'bool',
      'address',
      'signer',
      'bytearray',
    ),

    module_access: $ => choice(
      field('member', alias($._reserved_identifier, $.identifier)),
      field('member', $.identifier),
      seq(
        field('module', $._module_identifier),
        '::',
        field('member', $.identifier)
      ),
      seq(
        $.module_identity,
        '::',
        field('member', $.identifier)
      ),
    ),

    macro_module_access: $ => seq(field("access", $.module_access), "!"),


    module_identity: $ => seq(
      field('address', choice($.address_literal, $._module_identifier)),
      '::',
      field('module', $._module_identifier)
    ),

    type_arguments: $ => seq(
      '<',
      sepBy1(',', $._type),
      '>'
    ),

    function_type: $ => seq(
      field('param_types', $.function_type_parameters),
      field('return_type', $._type)
    ),
    function_type_parameters: $ => seq('|', sepBy(',', $._type), '|'),

    // function parameter grammar
    function_parameter: $ => seq(
      field('name', $._variable_identifier),
      ':',
      field('type', $._type),
    ),

    // type parameter grammar
    type_parameters: $ => seq('<', sepBy1(',', $.type_parameter), '>'),
    type_parameter: $ => seq(
      optional('phantom'),
      $._type_parameter_identifier,
      optional(seq(':', 
          sepBy1('+', $.ability)
          ))
    ),

    // Block

    block: $ => seq(
      '{',
      repeat($.use_declaration),
      repeat($.block_item),
      optional($._expression),
      '}'
    ),
    block_item: $ => seq(
      choice(
        $._expression,
        $.let_statement,
      ),
      ';'
    ),
    let_statement: $ => seq(
      'let',
      field('binds', $.bind_list),
      optional(seq(':', field('type', $._type))),
      optional(seq('=', field('expr', $._expression)))
    ),
    // Block end


    // Expression

    _expression: $ => choice(
      $.lambda_expression,
      $.if_expression,
      $.while_expression,
      $.loop_expression,
      $.return_expression,
      $.abort_expression,
      $.assign_expression,
      // unary expression is included in binary_op,
      $._unary_expression,
      $.binary_expression,
      $.quantifier_expression
    ),

    quantifier_expression: $ => prec.right(seq(
      choice($._forall, $._exists),
      $.quantifier_bindings,
      optional(seq('where', $._expression)),
      ':',
      $._expression
    )),
    quantifier_bindings: $ => sepBy1(',', $.quantifier_binding),
    quantifier_binding: $ => choice(
      seq($.identifier, ':', $._type),
      seq($.identifier, 'in', $._expression)
    ),
    lambda_expression: $ => seq(
      field('bindings', $.lambda_bindings),
      field('expr', $._expression)
    ),
    lambda_bindings: $ => seq(
      '|',
      sepBy(',', $._bind),
      '|'
    ),
    // if-else expression
    if_expression: $ => prec.right(
      seq(
        'if',
        '(',
        field('eb', $._expression),
        ')',
        field('et', $._expression),
        optional(seq(
          'else',
          field('ef', $._expression)
        )),
      )
    ),

    // while expression
    while_expression: $ => seq(
      'while',
      '(',
      field('eb', $._expression),
      ')',
      field('body', $._expression),
    ),

    // loop expression
    loop_expression: $ => seq('loop', field('body', $._expression)),

    // return expression
    return_expression: $ => seq('return', optional(field('return', $._expression))),

    // abort expression
    abort_expression: $ => seq('abort', field('abort', $._expression)),

    assign_expression: $ => prec.left(PRECEDENCE.assign,
      seq(
        field('lhs', $._unary_expression),
        '=',
        field('rhs', $._expression)
      )
    ),

    //      BinOpExp =
    //          <BinOpExp> <BinOp> <BinOpExp>
    //          | <UnaryExp>
    _binary_operand: $ => choice(
      $._unary_expression,
      $.binary_expression,
    ),
    binary_expression: $ => {
      const table = [
        [PRECEDENCE.implies, '==>'],
        [PRECEDENCE.or, '||'],
        [PRECEDENCE.and, '&&'],
        [PRECEDENCE.eq, '=='],
        [PRECEDENCE.neq, '!='],
        [PRECEDENCE.lt, '<'],
        [PRECEDENCE.gt, '>'],
        [PRECEDENCE.le, '<='],
        [PRECEDENCE.ge, '>='],
        [PRECEDENCE.range, '..'],
        [PRECEDENCE.bitor, '|'],
        [PRECEDENCE.xor, '^'],
        [PRECEDENCE.bitand, '&'],
        [PRECEDENCE.shl, '<<'],
        [PRECEDENCE.shr, '>>'],
        [PRECEDENCE.add, '+'],
        [PRECEDENCE.sub, '-'],
        [PRECEDENCE.mul, '*'],
        [PRECEDENCE.div, '/'],
        [PRECEDENCE.mod, '%']
      ];

      let binary_expression = choice(...table.map(
        ([precedence, operator]) => prec.left(precedence, seq(
          field('lhs', $._binary_operand),
          field('operator', alias(operator, $.binary_operator)),
          field('rhs', $._binary_operand),
        ))
      ));

      return binary_expression;
    },

    _unary_expression: $ => choice(
      $.unary_expression,
      $.borrow_expression,
      $.dereference_expression,
      $.move_or_copy_expression,
      $._expression_term,
    ),
    unary_expression: $ => seq(
      field('op', $.unary_op),
      field('expr', $._unary_expression)
    ),
    unary_op: $ => choice('!'),

    // dereference
    dereference_expression: $ => prec(PRECEDENCE.unary, seq(
      '*',
      field('expr', $._unary_expression),
    )),
    // borrow
    borrow_expression: $ => prec(PRECEDENCE.unary, seq(
      choice('&', '&mut'),
      field('expr', $._unary_expression),
    )),
    // move or copy
    move_or_copy_expression: $ => prec(PRECEDENCE.unary, seq(
      choice('move', 'copy'),
      field('expr', $._variable_identifier),
    )),

    _expression_term: $ => choice(
      $.break_expression,
      $.continue_expression,
      $.name_expression,
      $.call_expression,
      $.macro_call_expression,
      $.pack_expression,
      $._literal_value,
      $.unit_expression,
      $.expression_list,
      $.annotate_expression,
      $.cast_expression,
      $.block,
      $.spec_block,

      $.dot_expression,
      $.index_expression,
    ),
    break_expression: $ => choice('break'),
    continue_expression: $ => choice('continue'),
    name_expression: $ => seq(
      field('access', $.module_access),
      optional(field('type_arguments', $.type_arguments)),
    ),
    call_expression: $ => seq(
      field('access', $.module_access),
      optional(field('type_arguments', $.type_arguments)),
      field('args', $.arg_list),
    ),
    macro_call_expression: $ => seq(
      field('access', $.macro_module_access),
      optional(field('type_arguments', $.type_arguments)),
      field('args', $.arg_list),
    ),
    pack_expression: $ => seq(
      field('access', $.module_access),
      optional(field('type_arguments', $.type_arguments)),
      field('body', $.field_initialize_list),
    ),

    field_initialize_list: $ => seq(
      '{',
      sepBy(',', $.exp_field),
      '}'
    ),

    arg_list: $ => seq(
      '(',
      sepBy(',', $._expression),
      ')'
    ),

    expression_list: $ => seq('(', sepBy1(',', $._expression), ')'),
    unit_expression: $ => seq('(', ')'),
    cast_expression: $ => seq(
      '(',
      field('expr', $._expression),
      'as',
      field('ty', $._type),
      ')'
    ),
    annotate_expression: $ => seq(
      '(',
      field('expr', $._expression),
      ':',
      field('ty', $._type),
      ')'
    ),


    dot_expression: $ => prec.left(PRECEDENCE.field, seq(
      field('expr', $._expression_term),
      '.',
      field('field', $._field_identifier)
    )),
    index_expression: $ => prec.left(PRECEDENCE.call, seq(
      field('expr',
        $._expression_term,
      ),
      '[', field('idx', $._expression), ']'
    )),

    // Expression end

    // Fields and Bindings
    exp_field: $ => seq(
      field('field', $._field_identifier),
      optional(seq(
        ':',
        field('expr', $._expression)
      ))
    ),

    // The bindlist is enclosed in parenthesis, except that the parenthesis are
    // optional if there is a single Bind.
    bind_list: $ => choice(
      $._bind,
      seq('(', sepBy(',', $._bind), ')')
    ),
    _bind: $ => choice(
      alias($._variable_identifier, $.bind_var),
      $.bind_unpack,
    ),
    bind_unpack: $ => seq(
      $.module_access,
      optional(field('type_arguments', $.type_arguments)),
      field('bind_fields', $.bind_fields),
    ),
    bind_fields: $ => seq(
      '{', sepBy(',', $.bind_field), '}'
    ),
    bind_field: $ => seq(
      field('field', $._field_identifier), // direct bind
      optional(seq(
        ':',
        field('bind', $._bind)
      ))
    ),
    // Fields and Bindings - End

    // literals
    _literal_value: $ => choice(
      $.address_literal,
      $.bool_literal,
      $.num_literal,
      $.hex_string_literal,
      $.byte_string_literal,
      $.vector_literal,
    ),
    address_literal: $ => /@0x[a-fA-F0-9]+/,
    bool_literal: $ => choice('true', 'false'),
    num_literal: $ => choice(/[0-9][0-9_]*(?:u8|u16|u32|u64|u128|u256)?/, /0x[a-fA-F0-9_]+/),
    hex_string_literal: $ => /x"[0-9a-fA-F]*"/,
    byte_string_literal: $ => /b"(\\.|[^\\"])*"/,
    vector_literal: $ => seq("vector[", sepBy(",", $._literal_value), "]"),
    _module_identifier: $ => alias($.identifier, $.module_identifier),
    _struct_identifier: $ => alias($.identifier, $.struct_identifier),
    _function_identifier: $ => alias($.identifier, $.function_identifier),
    _variable_identifier: $ => alias($.identifier, $.variable_identifier),
    _field_identifier: $ => alias($.identifier, $.field_identifier),
    _type_parameter_identifier: $ => alias($.identifier, $.type_parameter_identifier),
    identifier: $ => /[a-zA-Z_][0-9a-zA-Z_]*/,
    macro_identifier: $ => /[a-zA-Z_][0-9a-zA-Z_]*!/,
    _reserved_identifier: $ => choice($._forall, $._exists),

    _forall: $ => 'forall',
    _exists: $ => 'exists',
    line_comment: $ => token(seq(
      '//', /.*/
    )),
    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    block_comment: $ => token(seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/'
    ))
  }
});

//      (<rule> 'sep')* <rule>?
// Note that this allows an optional trailing `sep`.
function sepBy(sep, rule) {
  return seq(repeat(seq(rule, sep)), optional(rule));
}
function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)), optional(sep));
}
