# Special Functions em CDS Views no ABAP

As *Special Functions* em CDS Views oferecem funcionalidades avançadas para realizar cálculos, formatações, e manipulações nos dados diretamente na camada de modelagem. Aqui está a lista completa de funções especiais disponíveis em CDS Views:

---

## 🎯 Conversões de Dados

### `CAST`
Converte um valor para um tipo de dado específico.

```cds
CAST(<valor> AS <tipo_dado>)
```


```cds
CAST(amount AS ABAP.NUMC(10)) AS amount_as_numc
```

### `CONVERT_CURRENCY`
Converte valores monetários entre diferentes moedas.

```cds
CONVERT_CURRENCY(<valor>, <moeda_origem>, <moeda_destino>)
```


```cds
CONVERT_CURRENCY(sales_amount, 'USD', 'EUR') AS converted_sales_amount
```

---

## 📅 Manipulação de Datas

### `DATS_ADD_DAYS`
Adiciona um número de dias a uma data.

```cds
DATS_ADD_DAYS(<data>, <dias>)
```


```cds
DATS_ADD_DAYS(order_date, 30) AS due_date
```

### `DATS_DIFF`
Calcula a diferença em dias entre duas datas.

```cds
DATS_DIFF(<data1>, <data2>)
```


```cds
DATS_DIFF(current_date, order_date) AS days_since_order
```

### `DATS_IS_VALID`
Verifica se uma data é válida.

```cds
DATS_IS_VALID(<data>)
```


```cds
DATS_IS_VALID('20250101') AS is_valid_date
```

---

## 🔢 Funções Numéricas

### `ROUND`
Arredonda um número para o número de casas decimais especificado.

```cds
ROUND(<número>, <casas_decimais>)
```


```cds
ROUND(total_price, 2) AS rounded_price
```

### `DIV`
Realiza a divisão inteira entre dois números.

```cds
<número1> DIV <número2>
```


```cds
quantity DIV pack_size AS full_packs
```

### `MOD`
Retorna o resto da divisão entre dois números.

```cds
<número1> MOD <número2>
```


```cds
quantity MOD pack_size AS remaining_items
```

### `CEIL`
Arredonda um número para o inteiro superior mais próximo.

```cds
CEIL(<número>)
```


```cds
CEIL(order_amount / pack_size) AS required_packs
```

### `FLOOR`
Arredonda um número para o inteiro inferior mais próximo.

```cds
FLOOR(<número>)
```


```cds
FLOOR(order_amount / pack_size) AS required_packs
```

---

## 🔠 Manipulação de Strings

### `CONCAT`
Concatena dois valores de texto.

```cds
CONCAT(<texto1>, <texto2>)
```


```cds
CONCAT(first_name, last_name) AS full_name
```

### `SUBSTRING`
Extrai uma substring de um texto.

```cds
SUBSTRING(<texto>, <posição_inicial>, <comprimento>)
```


```cds
SUBSTRING(product_code, 1, 3) AS product_prefix
```

### `LEFT`
Retorna os primeiros N caracteres de um texto.

```cds
LEFT(<texto>, <número_de_caracteres>)
```


```cds
LEFT(product_name, 5) AS product_short_name
```

### `RIGHT`
Retorna os últimos N caracteres de um texto.

```cds
RIGHT(<texto>, <número_de_caracteres>)
```


```cds
RIGHT(product_code, 4) AS product_suffix
```

### `LENGTH`
Retorna o comprimento de uma string.

```cds
LENGTH(<texto>)
```


```cds
LENGTH(customer_name) AS name_length
```

### `REPLACE`
Substitui uma parte do texto por outro valor.

```cds
REPLACE(<texto>, <antigo>, <novo>)
```


```cds
REPLACE(product_name, 'Old', 'New') AS updated_product_name
```

---

## 🔍 Funções de Pesquisa

### `UPPER`
Converte um texto para letras maiúsculas.

```cds
UPPER(<texto>)
```


```cds
UPPER(customer_name) AS customer_name_upper
```

### `LOWER`
Converte um texto para letras minúsculas.

```cds
LOWER(<texto>)
```


```cds
LOWER(customer_name) AS customer_name_lower
```

### `LIKE`
Verifica se um valor atende a um padrão especificado.

```cds
<campo> LIKE <padrão>
```


```cds
customer_name LIKE 'A%' AS starts_with_a
```

### `POSITION`
Retorna a posição da primeira ocorrência de uma substring em um texto.

```cds
POSITION(<substring> IN <texto>)
```


```cds
POSITION('-' IN product_code) AS hyphen_position
```

---

## 📊 Funções de Agregação

### `SUM`
Calcula a soma dos valores de uma coluna.

```cds
SUM(<coluna>)
```


```cds
SUM(order_amount) AS total_sales
```

### `AVG`
Calcula a média dos valores de uma coluna.

```cds
AVG(<coluna>)
```


```cds
AVG(delivery_time) AS avg_delivery_time
```

### `MIN`
Retorna o menor valor de uma coluna.

```cds
MIN(<coluna>)
```


```cds
MIN(order_date) AS earliest_order
```

### `MAX`
Retorna o maior valor de uma coluna.

```cds
MAX(<coluna>)
```


```cds
MAX(order_date) AS latest_order
```

### `COUNT`
Conta o número de registros em uma coluna.

```cds
COUNT(<coluna>)
```


```cds
COUNT(order_id) AS total_orders
```

---

## 🚀 Funções Avançadas

### `CASE`
Implementa lógica condicional dentro da view.

```cds
CASE
  WHEN <condição> THEN <valor1>
  ELSE <valor2>
END
```


```cds
CASE
  WHEN status = 'A' THEN 'Active'
  WHEN status = 'I' THEN 'Inactive'
  ELSE 'Unknown'
END AS status_description
```

### `COALESCE`
Retorna o primeiro valor não nulo de uma lista de valores.

```cds
COALESCE(<valor1>, <valor2>, ...)
```


```cds
COALESCE(phone_number, mobile_number, 'N/A') AS contact_number
```

### `ABS`
Retorna o valor absoluto de um número.

```cds
ABS(<número>)
```


```cds
ABS(balance) AS absolute_balance
```

### `SIGN`
Retorna o sinal de um número: -1, 0 ou 1.

```cds
SIGN(<número>)
```


```cds
SIGN(balance) AS balance_sign
```

---

### `CURRENCY_CONVERSION`
Realiza conversão de moeda com base na taxa de câmbio.

```cds
CURRENCY_CONVERSION(<valor>, <moeda_origem>, <moeda_destino>, <data>)
```


```cds
CURRENCY_CONVERSION(order_amount, 'USD', 'EUR', order_date) AS converted_amount
```
