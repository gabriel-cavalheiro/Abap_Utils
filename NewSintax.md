# Novos Comandos de Sintaxe ABAP no SAP 7.4 🚀

## 1. Inline Data Declarations ✍️
Permite declarar e inicializar variáveis em uma única linha.

**Antigo:**

~~~
DATA: lv_value TYPE string.
lv_value = 'Hello, World!'.
~~~
**Novo:**

~~~~
DATA(lv_value) = 'Hello, World!'.
~~~~

## 2. Inline Field-Symbols 🔄
Simplifica a declaração e atribuição de field-symbols.

**Antigo:**

~~~~
FIELD-SYMBOLS: <fs_value> TYPE any.
ASSIGN lt_table[ 1 ] TO <fs_value>.
~~~~

**Novo:**

~~~~
FIELD-SYMBOLS(<fs_value>) = <lt_table>[ 1 ].
~~~~

## 3. Inline Work Areas in Loops 🔄
Permite declarar a área de trabalho diretamente no loop.

**Antigo:**

~~~~
LOOP AT lt_table INTO ls_row.
  " Process ls_row
ENDLOOP.
~~~~

**Novo:**

~~~~
LOOP AT lt_table INTO DATA(ls_row).
  " Process ls_row
ENDLOOP.
~~~~

## 4. String Templates 🧩
Facilita a concatenação e formatação de strings.

**Antigo:**

~~~~
lv_string = 'The value is ' && lv_value.
~~~~

**Novo:**

~~~~
DATA(lv_string) = |The value is { lv_value }|.
~~~~

## 5. FOR Expressions 🔄
Permite transformar tabelas internas de forma concisa.

**Antigo:**

~~~~
LOOP AT lt_old_table INTO ls_old_row.
  APPEND ls_old_row TO lt_new_table.
ENDLOOP.
~~~~

**Novo:**

~~~~
DATA(lt_new_table) = VALUE #( FOR wa IN lt_old_table ( wa ) ).
~~~~

![image](https://github.com/user-attachments/assets/0e0b8b66-a996-496f-b2eb-d991b5466597)

Criando uma tabela a partir de outra:

![image](https://github.com/user-attachments/assets/92d821fd-df98-4417-b57d-82c351379694)

Com WHERE:

![image](https://github.com/user-attachments/assets/e32ab57f-efe2-45e7-bd40-2b8b5b0641b9)


## 6. FILTER Operator 🔍
Filtra tabelas internas de maneira direta.

**Antigo:**

~~~~
LOOP AT lt_table INTO ls_row WHERE field = 'value'.
  APPEND ls_row TO lt_filtered.
ENDLOOP.
~~~~

**Novo:**

~~~~
DATA(lt_filtered) = FILTER #( lt_table WHERE field = 'value' ).
~~~~

## 7. REDUCE Operator ➕
Realiza operações de agregação de forma declarativa.

**Antigo:**

~~~~
lv_total = 0.
LOOP AT lt_table INTO ls_row.
  lv_total = lv_total + ls_row-value.
ENDLOOP.
~~~~

**Novo:**

~~~~
DATA(lv_total) = REDUCE i( INIT sum = 0 FOR wa IN lt_table NEXT sum = sum + wa ).
~~~~

![image](https://github.com/user-attachments/assets/f2af4859-6851-4b35-88c4-e62dbd862ffe)

Exemplo prático:

![image](https://github.com/user-attachments/assets/40938266-90bf-4a9c-8ab1-7fc492f8088e)

![image](https://github.com/user-attachments/assets/81e0fafb-b61b-40cf-8211-c6fea1626a9c)

Com Where: 

![image](https://github.com/user-attachments/assets/30bed9da-7fb4-44c1-929f-de29fb13fa9f)

![image](https://github.com/user-attachments/assets/524ff551-6557-4e95-9144-fa12be7b1ed5)

Tabela dentro de tabela:

![image](https://github.com/user-attachments/assets/68f0af97-101d-4d16-ab11-274fcc52c4c5)

![image](https://github.com/user-attachments/assets/330223d7-8eb0-45b5-8886-dfcf949faaa0)

Resultado de tipo estruturado:

![image](https://github.com/user-attachments/assets/61ba9330-173c-4e7c-8a84-9cdfca9678d3)

Exemplo fatorial, sem uso de tabela interna:

![image](https://github.com/user-attachments/assets/5ddbd489-4440-4cf1-a93f-99f2099a498b)


## 8. New Open SQL Syntax 📜
Permite a declaração inline de variáveis em comandos SQL.

**Antigo:**

~~~~
SELECT * FROM scarr INTO TABLE lt_scarr.
~~~~

**Novo:**

~~~~
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
~~~~

## 9. CASE Expressions in SQL 🔄
Adiciona lógica condicional diretamente nas consultas SQL.

~~~~
SELECT carrid,
       CASE WHEN mandt = '100' THEN 'Domestic' ELSE 'International' END AS flight_type
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
~~~~

## 10. UP TO n ROWS Addition 🔢
Limita o número de linhas retornadas por um SELECT.

~~~~
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr) UP TO 10 ROWS.
~~~~

## 11. LOOP AT with Grouping 📊
Facilita o processamento de dados agrupados em loops.

**Antigo:**

~~~~
SORT lt_table BY category.
LOOP AT lt_table INTO ls_row.
  AT NEW category.
    " New category group starts
  ENDAT.
  " Process ls_row
ENDLOOP.
~~~~

**Novo:**

~~~~
LOOP AT lt_table INTO DATA(ls_row)
     GROUP BY ls_row-category
     INTO DATA(ls_group).
  LOOP AT GROUP ls_group INTO DATA(ls_group_row).
    " Process group rows
  ENDLOOP.
ENDLOOP.
~~~~

## 12. Switch Expressions 🔄
Simplifica seleções condicionais.

**Antigo:**

~~~~
IF lv_input = 'A'.
  lv_result = 'Alpha'.
ELSEIF lv_input = 'B'.
  lv_result = 'Beta'.
ELSE.
  lv_result = 'Unknown'.
ENDIF.
~~~~

**Novo:**

~~~~
DATA(lv_result) = SWITCH #( lv_input
                            WHEN 'A' THEN 'Alpha'
                            WHEN 'B' THEN 'Beta'
                            ELSE 'Unknown' ).
~~~~

Uso em entrada de método:

![image](https://github.com/user-attachments/assets/0eef27d8-a8f2-4376-9581-56ad1986dc93)

Retorno de método:

![image](https://github.com/user-attachments/assets/41e1d195-b6a3-4fa8-8449-919d37926eec)

Uso com Value:

![image](https://github.com/user-attachments/assets/dcdf1e6b-e637-45c0-8e81-549954b52404)

## 13. COND Operator ⚙️
Cria expressões condicionais de forma declarativa.

**Antigo:**

~~~~
IF lv_input = 'A'.
  lv_result = 'Alpha'.
ELSEIF lv_input = 'B'.
  lv_result = 'Beta'.
ELSE.
  lv_result = 'Unknown'.
ENDIF.
~~~~

**Novo:**

~~~~
DATA(lv_result) = COND #( WHEN lv_input = 'A' THEN 'Alpha'
                          WHEN lv_input = 'B' THEN 'Beta'
                          ELSE 'Unknown' ).
~~~~

Uso em entrada de método:

![image](https://github.com/user-attachments/assets/cb9a7b4f-3447-4884-a593-c179cd9d0a6e)

retorno de método:

![image](https://github.com/user-attachments/assets/f3ab5983-1e20-4513-8680-045ad2de97ff)

Preenchimento com Value:

![image](https://github.com/user-attachments/assets/2b595813-a412-4446-b6d5-5271130bfc47)


## 14. NEW Operator 🌟
Simplifica a criação de instâncias de objetos.

**Antigo:**

~~~~
CREATE OBJECT lo_instance TYPE cl_class.
~~~~

**Novo:**

~~~~
DATA(lo_instance) = NEW cl_class( ).
~~~~

## 15. VALUE Operator for Structures and Tables 🏗️
Inicializa estruturas e tabelas de forma declarativa.

**Antigo:**

~~~~
CLEAR ls_struct.
ls_struct-field1 = 'value1'.
ls_struct-field2 = 'value2'.
APPEND ls_struct TO lt_table.
~~~~

**Novo:**

~~~~
DATA(ls_struct) = VALUE #( field1 = 'value1' field2 = 'value2' ).
DATA(lt_table) = VALUE #( ( field1 = 'value1' field2 = 'value2' )
                          ( field1 = 'value3' field2 = 'value4' ) ).
~~~~
Em Estruturas:

![image](https://github.com/user-attachments/assets/b39a15b8-7f45-45b3-b99d-fa5288bcc930)

Em tabelas Internas:

![image](https://github.com/user-attachments/assets/78b46186-4d61-403f-8a74-0a9a3f150de3)

Tabela Interna com Valor Default:

![image](https://github.com/user-attachments/assets/e6d0f7be-d806-49ce-90e6-4568e3d8255a)

Uso como parâmetro de método:

![image](https://github.com/user-attachments/assets/a8221e94-1f1d-4d50-a893-b31ab9f0dea2)

Montado Range:

![image](https://github.com/user-attachments/assets/7d49e61b-c05a-4fe9-8e95-b2e1665aadbf)

Uso Append:

![image](https://github.com/user-attachments/assets/3f91f6fd-9cbb-4456-b571-0eb3b2475fcb)

## 16. String Functions in SQL 🔤
Realiza operações em colunas de texto diretamente no SQL.

~~~~
SELECT carrid, LENGTH( carrname ) AS carrname_length
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
~~~~

## 17. Built-in Functions in SQL ⚙️
Permite manipulações diretas de dados no SQL.

**Antigo:**

~~~~
SELECT carrid, LEFT( carrname, 3 ) AS carrname_short
  FROM scarr
  INTO TABLE lt_scarr.
~~~~

**Novo:**

~~~~
SELECT carrid, LEFT( carrname, 3 ) AS carrname_short
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
~~~~

## 18. Escaping Host Variables with @ 🛡️
Evita conflitos de escopo em variáveis de host no SQL.

**Antigo:**

~~~~
SELECT * FROM scarr INTO TABLE lt_scarr WHERE carrid = lv_carrid.
~~~~

**Novo:**

~~~~
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr) WHERE carrid = @lv_carrid.
~~~~

## 19. Escape Character for Literals 🔡
Facilita a inclusão de caracteres especiais em strings.

**Antigo:**

~~~~
lv_string = 'This is a string with a single backslash \'.
~~~~

**Novo:**

~~~~
DATA(lv_string) = `This is a string with a single backslash \`.
~~~~

## 20. XSDBOOL 🧮
Converte expressões booleanas de forma direta.

**Antigo:**

~~~~
IF lv_value = 'X'.
  lv_bool = abap_true.
ELSE.
  lv_bool = abap_false.
ENDIF.
~~~~

**Novo:**

~~~~
DATA(lv_bool) = xsdbool( lv_value = 'X' ).
~~~~

## 21. CORRESPONDING 🔄
Copia estruturas com nomes de campos correspondentes.

**Antigo:**

~~~~
MOVE-CORRESPONDING ls_old TO ls_new.
~~~~

**Novo:**

~~~~
DATA(ls_new) = CORRESPONDING #( ls_old ).
~~~~

Manter valor não correpondente:

![image](https://github.com/user-attachments/assets/0dd0a466-9da5-41c1-b122-d1826f3ddf04)

Mapping:

![image](https://github.com/user-attachments/assets/faeb125c-1df7-41ea-9760-634c141a3432)

Except para não ver um campo específico:

![image](https://github.com/user-attachments/assets/3e63dbec-2b19-49a6-a123-0d6db53b021b)

Remover Duplicados:

![image](https://github.com/user-attachments/assets/d5d00452-205e-4895-9d6f-d727aa950992)


## 22. LINES() 📏
Retorna o número de linhas de uma tabela interna.

**Antigo:**

~~~~
DESCRIBE TABLE lt_table LINES lv_lines.
~~~~

**Novo:**

~~~~
DATA(lv_lines) = LINES( lt_table ).
~~~~

## 23. LINE_EXISTS() ✅
Verifica se uma linha existe na tabela interna.

**Antigo:**

~~~~
READ TABLE lt_table INDEX 1 INTO ls_row TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  " Do something
ENDIF.
~~~~

**Novo:**

~~~~
IF line_exists( lt_table[ 1 ] ).
  " Do something
ENDIF.
~~~~

## 24. BOOLC() 🔲
Retorna 'X' ou espaço com base em uma expressão booleana.

**Antigo:**

~~~~
IF lv_value = 'X'.
  lv_bool = 'X'.
ELSE.
  lv_bool = ''.
ENDIF.
~~~~

**Novo:**

~~~~
DATA(lv_bool) = boolc( lv_value = 'X' ).
~~~~

## 25. CONV 🔄
Converte valores entre diferentes tipos de dados.

**Antigo:**

~~~~
lv_int = lv_string+0.
~~~~

**Novo:**

~~~~
DATA(lv_int) = CONV i( lv_string ).
~~~~

![image](https://github.com/user-attachments/assets/d0246408-6a5a-49b4-ab85-4b732f8354f1)

## 26. CAST 🔄
Converte explicitamente classes em hierarquias de herança.

**Antigo:**

~~~~
lo_super ?= lo_sub.
~~~~

**Novo:**

~~~~
DATA(lo_super) = CAST super( lo_sub ).
~~~~

## 27. REF 🔗
Cria referências a objetos de forma direta.

**Antigo:**

~~~~
GET REFERENCE OF ls_structure INTO lo_obj.
~~~~

**Novo:**

~~~~
DATA(lo_obj) = REF #( ls_structure ).
~~~~

## 28. SHIFT LEFT/RIGHT ↔️
Move caracteres em uma string.


~~~~
SHIFT lv_string LEFT DELETING LEADING space.
~~~~


## 29. CONDENSE 🧼
Remove espaços em branco de strings.


~~~~
CONDENSE lv_string NO-GAPS.
~~~~


## 30. TRANSLATE 🔠
Converte maiúsculas/minúsculas em strings.


~~~~
TRANSLATE lv_string TO UPPER CASE.
~~~~


## 31. SPLIT ✂️
Divide strings em partes com base em delimitadores.

~~~~
SPLIT lv_string AT space INTO TABLE lt_tokens.
~~~~
