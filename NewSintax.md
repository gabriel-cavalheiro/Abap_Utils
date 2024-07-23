# Novos Comandos de Sintaxe ABAP no SAP 7.4

## 1. Inline Data Declarations
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

## 2. Inline Field-Symbols
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

## 3. Inline Work Areas in Loops

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

## 4. String Templates

**Antigo:**

```
lv_string = 'The value is ' && lv_value.
```

**Novo:**

```
DATA(lv_string) = |The value is { lv_value }|.
```
