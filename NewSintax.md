# Novos Comandos de Sintaxe ABAP no SAP 7.4 🚀

## 1. Inline Data Declarations ✍️
Permite declarar e inicializar variáveis em uma única linha.

**Antigo:**

```abap
DATA: lv_value TYPE string.
lv_value = 'Hello, World!'.
```
**Novo:**

```abap
DATA(lv_value) = 'Hello, World!'.
```

## 2. Inline Field-Symbols 🔄
Simplifica a declaração e atribuição de field-symbols.

**Antigo:**

```abap
FIELD-SYMBOLS: <fs_value> TYPE any.
ASSIGN lt_table[ 1 ] TO <fs_value>.
```

**Novo:**

```abap
FIELD-SYMBOLS(<fs_value>) = <lt_table>[ 1 ].
```

## 3. Inline Work Areas in Loops 🔄
Permite declarar a área de trabalho diretamente no loop.

**Antigo:**

```abap
LOOP AT lt_table INTO ls_row.
  " Process ls_row
ENDLOOP.
```

**Novo:**

```abap
LOOP AT lt_table INTO DATA(ls_row).
  " Process ls_row
ENDLOOP.
```

## 4. String Templates 🧩
Facilita a concatenação e formatação de strings.

**Antigo:**

```abap
lv_string = 'O valor é ' && lv_value.
```

**Novo:**

```abap
DATA(lv_string) = |O valor é { lv_value }|.
```

## 5. FOR Expressions 🔄
Permite transformar tabelas internas de forma concisa.

**Antigo:**

```abap
LOOP AT lt_old_table INTO ls_old_row.
  APPEND ls_old_row TO lt_new_table.
ENDLOOP.
```

**Novo:**

```abap
DATA(lt_new_table) = VALUE #( FOR wa IN lt_old_table ( wa ) ).
```

```abap
" Sem FOR:
LOOP AT t_order INTO DATA(s_order).
  APPEND VALUE y_customer( kunnr = s_order-kunnr )
  TO t_customer.
ENDLOOP.

" Com FOR:
t_customer =
  VALUE y_t_customer(
    FOR s_order IN t_order 
    ( kunnr = s_order-kunnr )
  ).
```
**Criando uma tabela a partir de outra:**

```abap
" Loop como instrução:
LOOP AT t_order INTO DATA(s_order)
  APPEND
   value y_customer(kunnr = s_order-kunnr)
TO t_customer.
ENDLOOP.

" FOR: Loop como expressão:
t_customer =
  VALUE y_t_customer(
    FOR s_order IN t_order( kunnr = s_order-kunnr )
  ).
```
**Com WHERE:**

```abap
" Loop como instrução:
LOOP AT t_order INTO DATA(s_order)
  WHERE AUART = 'AB'.
  APPEND VALUE y_customer( kunnr = s_order-kunnr )
  TO t_customer.
ENDLOOP.

" FOR: Loop como expressão:
t_customer =
  VALUE y_t_customer(
    FOR s_order IN t_order
    WHERE ( AUART = 'AB' )
    ( kunnr = s_order-kunnr )
  ).
```

## 6. FILTER Operator 🔍
Filtra tabelas internas de maneira direta.

**Antigo:**

```abap
LOOP AT lt_table INTO ls_row WHERE field = 'value'.
  APPEND ls_row TO lt_filtered.
ENDLOOP.
```

**Novo:**

```abap
DATA(lt_filtered) = FILTER #( lt_table WHERE field = 'value' ).
```

**Exemplo com EXCEPT:**

```abap
DATA t_filter TYPE SORTED TABLE OF y_kna1-kunnr WITH UNIQUE KEY table_line.

t_filter = VALUE #(
 ('0000000010')
).

DATA(t_20e30) =
  FILTER #(
    t_kna1 EXCEPT IN t_filter
    WHERE kunnr = table_line
).
```

## 7. REDUCE Operator ➕
Realiza operações de agregação de forma declarativa.

**Antigo:**

```abap
lv_total = 0.
LOOP AT lt_table INTO ls_row.
  lv_total = lv_total + ls_row-value.
ENDLOOP.
```

**Novo:**

```abap
DATA(lv_total) = REDUCE i( INIT sum = 0 FOR wa IN lt_table NEXT sum = sum + wa ).
```

**Exemplo prático:**

![image](https://github.com/user-attachments/assets/40938266-90bf-4a9c-8ab1-7fc492f8088e)

**Sem Reduce:**

```abap
DATA v_tot_netwr TYPE y_order-netwr.

LOOP AT t_order INTO DATA(s_order).
  v_tot_netwr += s_order-netwr.
ENDLOOP.
```

**Com Reduce:**

```abap
DATA(v_tot_netwr) =
  REDUCE y_order-netwr(
    INIT x = 0
    FOR wa IN t_order
    NEXT x = x + wa-netwr
).
```

Com Where: 

![image](https://github.com/user-attachments/assets/30bed9da-7fb4-44c1-929f-de29fb13fa9f)

**Sem Reduce:**

```abap
DATA v_tot_netwr TYPE y_order-netwr.

LOOP AT t_order INTO DATA(s_order).
  WHERE auart = 'A'.
  v_tot_netwr += s_order-netwr.
ENDLOOP.
```

**Com Reduce:**

```abap
DATA(v_tot_netwr) =
  REDUCE y_order-netwr(
    INIT x = 0
    FOR wa IN t_order
    WHERE (auart = 'A')
    NEXT x = x + wa-netwr
).
```

Tabela dentro de tabela:

![image](https://github.com/user-attachments/assets/68f0af97-101d-4d16-ab11-274fcc52c4c5)

**Sem Reduce:**

```abap
DATA v_tot_netwr TYPE y_order-netwr.

LOOP AT t_order INTO DATA(s_order).
  WHERE auart = 'A'.

LOOP AT s_order-items INTO DATA(s_item).
  v_tot_netwr += s_order-netwr.
  ENDLOOP.
ENDLOOP.
```

**Com Reduce:**

```abap
DATA(v_tot_netwr) =
  REDUCE y_order-netwr(
    INIT x = 0
    FOR wa IN t_order
    WHERE (auart = 'A')
    FOR item IN order-items
    NEXT result = result + item-netwr
).
```

Resultado de tipo estruturado:

**Sem Reduce:**

```abap
DATA s_tot TYPE y_result.

LOOP AT t_order INTO DATA(s_order).
  WHERE auart = 'A'.

LOOP AT s_order-items INTO DATA(s_item).
  v_tot_netwr += s_order-netwr.
  s_tot-counter += 1.
  ENDLOOP.
ENDLOOP.
```

**Com Reduce:**

```abap
DATA(s_tot) =
  REDUCE y_result(
    INIT result =
      VALUE y_result(
        netwr = 0
        counter = 0
      )
    FOR wa IN t_order
    WHERE (auart = 'A')
    FOR item IN order-items
    NEXT
      result-netwr = result-netwr + item-netwr
      result-counter += 1
).
```

Exemplo fatorial, sem uso de tabela interna:

![image](https://github.com/user-attachments/assets/5ddbd489-4440-4cf1-a93f-99f2099a498b)


## 8. New Open SQL Syntax 📜
Permite a declaração inline de variáveis em comandos SQL.

**Antigo:**

```abap
SELECT * FROM scarr INTO TABLE lt_scarr.
```

**Novo:**

```abap
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
```

## 9. CASE Expressions in SQL 🔄
Adiciona lógica condicional diretamente nas consultas SQL.

```abap
SELECT carrid,
       CASE WHEN mandt = '100' THEN 'Domestic' ELSE 'International' END AS flight_type
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
```

## 10. UP TO n ROWS Addition 🔢
Limita o número de linhas retornadas por um SELECT.

```abap
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr) UP TO 10 ROWS.
```

## 11. LOOP AT with Grouping 📊
Facilita o processamento de dados agrupados em loops.

**Antigo:**

```abap
SORT lt_table BY category.
LOOP AT lt_table INTO ls_row.
  AT NEW category.
    " New category group starts
  ENDAT.
  " Process ls_row
ENDLOOP.
```

**Novo:**

```abap
LOOP AT lt_table INTO DATA(ls_row)
     GROUP BY ls_row-category
     INTO DATA(ls_group).
  LOOP AT GROUP ls_group INTO DATA(ls_group_row).
    " Process group rows
  ENDLOOP.
ENDLOOP.
```

## 12. Switch Expressions 🔄
Simplifica seleções condicionais.

**Antigo:**

```abap
IF lv_input = 'A'.
  lv_result = 'Alpha'.
ELSEIF lv_input = 'B'.
  lv_result = 'Beta'.
ELSE.
  lv_result = 'Unknown'.
ENDIF.
```

**Novo:**

```abap
DATA(lv_result) = SWITCH #( lv_input
                            WHEN 'A' THEN 'Alpha'
                            WHEN 'B' THEN 'Beta'
                            ELSE 'Unknown' ).
```

Uso em entrada de método:

**Antes:**
```abap

CASE v_promocao.
  WHEN 'X'.
    v_mode = 'A'.
  WHEN ''.
    v_mode = 'B'.
ENDCASE.

v_com_desc=
  desconto(
    iv_price = v_cheio
    iv_mode  = v_mode

```

**Depois:**
```abap

v_com_desc=
  desconto(
    iv_price = v_cheio
    iv_mode  = SWITCH #(
        v_promocao
        WHEN 'X' THEN 'A'
        WHEN '' THEN  'B'
      )
).
```

Retorno de método:

**Antes:**
```abap

CASE get_mode( ).
  WHEN 'A'.
    v_com_desc = v_cheio * '0.8'.
  WHEN OTHERS.
    v_com_desc = v_cheio * '0.9'.
ENDCASE.
```
<H3><b>Depois:</b></H3>

```abap

v_com_desc = SWITCH #(
        get_mode( ).
        WHEN 'A' THEN v_cheio * '0.8'
        ELSE v_ceio * '0.9
      ).
```
<h3><b>Uso com Value:</b></h3>

<h4><b>Antes:</b></h4>

```abap
CASE v_vlr.
  WHEN 1.
    DATA(s_entry1) =
      VALUE y_entry(
        valor = v_vlr
        descr = 'Um'
      ).
  WHEN 2.
    s_entry1 =
      VALUE y_entry(
        valor = v_vlr
        descr = 'Dois'
      ).
ENDCASE.

```

<h4><b>Novo:</b></h4>

```abap
DATA(s_entry1) =
  VALUE y_entry(
    valor = v_vlr
    descr =
      SWITCH #(
        v_vlr
        WHEN 1 THEN 'Um'
        WHEN 2 THEN 'Dois'
      )
).
```


## 13. COND Operator ⚙️
Cria expressões condicionais de forma declarativa.

**Antigo:**

```abap
IF lv_input = 'A'.
  lv_result = 'Alpha'.
ELSEIF lv_input = 'B'.
  lv_result = 'Beta'.
ELSE.
  lv_result = 'Unknown'.
ENDIF.
```

**Novo:**

```abap
DATA(lv_result) = COND #( WHEN lv_input = 'A' THEN 'Alpha'
                          WHEN lv_input = 'B' THEN 'Beta'
                          ELSE 'Unknown' ).
```

<h3>
  <b>Uso em entrada de método:</b>
</h3>

<h4><b>Antes:</b></h4>

```abap
DATA v_cheio TYPE f.
DATA v_com_desc TYPE f.
v_cheio = 150.
IF ( v_cheio > 100 ).
  v_com_desc = desconto(
    iv_price = v_cheio
    iv_mode = 'A'
  ).
ELSE.
  v_com_desc = desconto(
    iv_price = v_cheio
    iv_mode = 'B'
  ).
ENDIF.
```

<h4><b>Novo:</b></h4>

```abap
DATA v_cheio TYPE f.
v_cheio = 150.
DATA(v_com_desc) = desconto(
  iv_price = v_cheio
  iv_mode = COND #(
    WHEN v_cheio > 100
    THEN 'A' 
    ELSE 'B'
  )
).

```

<h3>
  <b>retorno de método:</b>
</h3>

<h4><b>Antes:</b></h4>

```abap
DATA v_cheio TYPE f.
DATA v_com_desc TYPE f.

v_cheio = 150.

IF get_mode( ) = 'A'.
  v_com_desc = v_cheio * '0.8'.
ELSE.
  v_com_desc = v_cheio * '0.9'.
ENDIF.
```

<h4><b>Novo:</b></h4>

```abap
DATA v_cheio TYPE f.
DATA v_com_desc TYPE f.

v_cheio = 150.

v_com_desc =
  COND #(
    WHEN get_mode( ) = 'A'
    THEN v_cheio * '0.8'
    ELSE v_cheio * '0.9'
  ).
```

<h3>
  <b>Preenchimento com Value:</b>
</h3>

<h4><b>Antes:</b></h4>

```abap
IF ( v_valor1 MOD 2 = 0 ).
  DATA(s_entry1) = VALUE y_entry(
    valor = v_valor1
    tipo  = 'Par'
  ).
ELSE.
  s_entry1 = VALUE y_entry(
    valor = v_valor1
    tipo  = 'Impar'
  ).
ENDIF.

```

<h4><b>Novo:</b></h4>

```abap
DATA(s_entry1) = 
  VALUE y_entry(
    valor = v_valor1
    tipo  = COND #(
      WHEN v_valor1 MOD 2 = 0
      THEN 'Par' 
      ELSE 'Impar'
    )
  ).
```

## 14. NEW Operator 🌟
Simplifica a criação de instâncias de objetos.

**Antigo:**

```abap
CREATE OBJECT lo_instance TYPE cl_class.
```

**Novo:**

```abap
DATA(lo_instance) = NEW cl_class( ).
```

## 15. VALUE Operator for Structures and Tables 🏗️
Inicializa estruturas e tabelas de forma declarativa.

**Antigo:**

```abap
CLEAR ls_struct.
ls_struct-field1 = 'value1'.
ls_struct-field2 = 'value2'.
APPEND ls_struct TO lt_table.
```

**Novo:**

```abap
DATA(ls_struct) = VALUE #( field1 = 'value1' field2 = 'value2' ).
DATA(lt_table) = VALUE #( ( field1 = 'value1' field2 = 'value2' )
                          ( field1 = 'value3' field2 = 'value4' ) ).
```
**Em Estruturas:**

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

```abap
SELECT carrid, LENGTH( carrname ) AS carrname_length
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
```

## 19. Escape Character for Literals 🔡
Facilita a inclusão de caracteres especiais em strings.

**Antigo:**

```abap
lv_string = 'This is a string with a single backslash \'.
```

**Novo:**

```abap
DATA(lv_string) = `This is a string with a single backslash \`.
```

## 20. XSDBOOL 🧮
Converte expressões lógicas em valores booleanos.

**Antigo:**

```abap
DATA lv_result TYPE abap_bool.

IF 5 > 3.
  lv_result = abap_true.
ELSE.
  lv_result = abap_false.
ENDIF.
WRITE: / 'Resultado:', lv_result.
```

**Novo:**

```abap
DATA(lv_result) = xsdbool( 5 > 3 ).

WRITE: / 'Resultado:', lv_result.
```

## 21. CORRESPONDING 🔄
Copia estruturas com nomes de campos correspondentes.

**Antigo:**

```abap
MOVE-CORRESPONDING ls_old TO ls_new.
```

**Novo:**

```abap
DATA(ls_new) = CORRESPONDING #( ls_old ).
```

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

```abap
DESCRIBE TABLE lt_table LINES lv_lines.
```

**Novo:**

```abap
DATA(lv_lines) = LINES( lt_table ).
```

## 23. LINE_EXISTS() ✅
Verifica se uma linha existe na tabela interna.

**Antigo:**

```abap
READ TABLE lt_table INDEX 1 INTO ls_row TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  " Do something
ENDIF.
```

**Novo:**

```abap
IF line_exists( lt_table[ 1 ] ).
  " Do something
ENDIF.
```

**Exemplo com COND:**

```abap
DATA(v_exist) = COND #(
  WHEN line_exists(t_kna1[kunnr = '20'])
  THEN 'Sim'
  ELSE 'Não'
).
```

## 25. line_index()
Retorna o sy-tabix da tabela interna com expressão.

**Antigo:**
```abap
READ TABLE t+kna1 TRANSPORTING NO FIELDS WITH KEY kunnr = '20'.

DATA(v_tabix) = sy-tabix.
```
**Novo:**

```abap
DATA(v_tabix) = line_index(t_kna1[kunnr = '20']).
```

## 24. BOOLC() 🔲
Retorna 'X' ou espaço com base em uma expressão booleana.

**Antigo:**

```abap
IF lv_value = 'X'.
  lv_bool = 'X'.
ELSE.
  lv_bool = ''.
ENDIF.
```

**Novo:**

```abap
DATA(lv_bool) = boolc( lv_value = 'X' ).
```

## 25. CONV 🔄
Converte valores entre diferentes tipos de dados.

**Antigo:**

```abap
lv_int = lv_string+0.
```

**Novo:**

```abap
DATA(lv_int) = CONV i( lv_string ).
```

![image](https://github.com/user-attachments/assets/d0246408-6a5a-49b4-ab85-4b732f8354f1)

## 26. CAST 🔄
Converte explicitamente classes em hierarquias de herança.

**Antigo:**

```abap
lo_super ?= lo_sub.
```

**Novo:**

```abap
DATA(lo_super) = CAST super( lo_sub ).
```

## 27. REF 🔗
Cria referências a objetos de forma direta.

**Antigo:**

```abap
GET REFERENCE OF ls_structure INTO lo_obj.
```

**Novo:**

```abap
DATA(lo_obj) = REF #( ls_structure ).
```

## 28. SHIFT LEFT/RIGHT ↔️
Move caracteres em uma string.

```abap
SHIFT lv_string LEFT DELETING LEADING space.
```


## 29. CONDENSE 🧼
Remove espaços em branco de strings.

```abap
CONDENSE lv_string NO-GAPS.
```


## 30. TRANSLATE 🔠
Converte maiúsculas/minúsculas em strings.

```abap
TRANSLATE lv_string TO UPPER CASE.
```


## 31. SPLIT ✂️
Divide strings em partes com base em delimitadores.

```abap
SPLIT lv_string AT space INTO TABLE lt_tokens.
```

## 32. LET
Permite criar variavel auxiliar apenas no escopo de uma expressão.

```abap
DATA(cinco) =
  CONV i(
    LET dois = 2
        tres = 3
    IN dois + tres
).
```

Exemplo com field-symbol:

```abap
DATA(v3) = VALUE y_pot(
  LET <v1> = t_val[1]-val
       v2 = (<v1> + v2)
). 
```

## 32. TABLE EXPRESSION
Um READ TABLE em forma de expressão.

Antigo:
```abap
READ TABLE t_kna1 INTO DATA(ls_10) WITH KEY kunnr ='10'.
```
Novo:
```abap
DATA(s_dez) = t_kna1[kunnr = '10'].
```

Exemplos de Uso:

```abap
*Sem Expressions
READ TABLE t_kna1 INTO DATA(ls_dez) WITH KEY kunnr ='10'.

READ TABLE t_kna1 INTO DATA(ls_vinte) INDEX 2.

READ TABLE t_kna1 INTO DATA(ls_none) INDEX 3.
IF (sy-subrc <> 0).
...
ENDIF.

*Com Expressions
DATA(s_dez) = t_kna1[kunnr = '10'].

DATA(s_vinte) = t_kna1[2].

TRY.
  DATA(s_none) = t_kna1[3].
CATCH cx_sy_itab_line_not_found INTO DATA(ex).
...
ENDTRY.
```

```abap
DATA(s_10) = t_kna1[kunnr = '10'].
DATA(s_10_name1) = s_10-name1.

*É o mesmo que:
DATA(s10_name) = t_kna1[kunnr ='10']-name1.
```

```abap
*Tabela Dentro de Tabela:

DATA(v_2002_netwr) = t_kna1[kunnr = '20']-t_orders[vbeln = '2002']-netwr.

```
