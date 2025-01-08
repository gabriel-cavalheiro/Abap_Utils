# 📜 Lista de Funções Scalar mais Utilizadas em CDS

No mundo dos **Core Data Services (CDS)** no SAP ABAP, as funções scalar são ferramentas poderosas para manipular e transformar dados diretamente no banco de dados. Abaixo você encontra uma lista das funções scalar mais comuns, organizadas por categorias para facilitar a consulta:

---

## 🧮 **Funções Matemáticas**
Essas funções são úteis para cálculos matemáticos básicos:

- **`ABS`**: Retorna o valor absoluto de um número.
  
```cds
    ABS( -5 ) -- Resultado: 5
```
- **`CEIL`**: Arredonda o número para o inteiro superior mais próximo.
  
```cds
    CEIL( 4.2 ) -- Resultado: 5
```
- **`FLOOR`**: Arredonda o número para o inteiro inferior mais próximo.
  
```cds
    FLOOR( 4.8 ) -- Resultado: 4
```
- **`ROUND`**: Arredonda o número para um número especificado de casas decimais.
  
```cds
    ROUND( 4.567, 2 ) -- Resultado: 4.57
```
- **`MOD`**: Retorna o resto da divisão entre dois números.
  
```cds
    MOD( 10, 3 ) -- Resultado: 1
```

---

## 🕒 **Funções de Data e Hora**
Essas funções ajudam a lidar com cálculos e manipulações de datas e horários:

- **`CURRENT_DATE`**: Retorna a data atual do sistema.
- **`CURRENT_TIME`**: Retorna o horário atual do sistema.
- **`DAYS_BETWEEN`**: Calcula o número de dias entre duas datas.
  
```cds
    DAYS_BETWEEN( '2024-01-01', '2024-01-08' ) -- Resultado: 7
```
- **`SECONDS_BETWEEN`**: Calcula o número de segundos entre dois timestamps.
- **`ADD_DAYS`**: Adiciona um número específico de dias a uma data.
  
```cds
    ADD_DAYS( CURRENT_DATE, 7 )
```
- **`ADD_YEARS`**: Adiciona um número específico de anos a uma data.
- **`CONVERT_DATE`**: Converte uma string para o formato de data.
  
```cds
    CONVERT_DATE( '2024-12-25' )
 ```

---

## 🔤 **Funções de String**
Manipule strings facilmente com essas funções:

- **`CONCAT`**: Junta duas strings em uma só.
  
```cds
    CONCAT( 'SAP', ' CDS' ) -- Resultado: 'SAP CDS'
```
- **`UPPER`**: Converte todos os caracteres de uma string para maiúsculas.
  
```cds
    UPPER( 'sap' ) -- Resultado: 'SAP'
```
- **`LOWER`**: Converte todos os caracteres de uma string para minúsculas.
  
```cds
    LOWER( 'CDS' ) -- Resultado: 'cds'
```
- **`LENGTH`**: Retorna o número de caracteres em uma string.
  
```cds
    LENGTH( 'SAP' ) -- Resultado: 3
```
- **`SUBSTRING`**: Extrai uma parte de uma string.
  
```cds
    SUBSTRING( 'ABAP CDS', 1, 4 ) -- Resultado: 'ABAP'
```
- **`REPLACE`**: Substitui uma substring por outra.
  
```cds
    REPLACE( 'SAP CDS', 'SAP', 'HANA' ) -- Resultado: 'HANA CDS'
```
- **`TRIM`**: Remove espaços em branco no início e no final de uma string.
  
```cds
    TRIM( '  ABAP  ' ) -- Resultado: 'ABAP'
```

---

## 🔍 **Funções Lógicas e Condicionais**
Estas funções são úteis para tomadas de decisão e validações dentro de views:

- **`CASE`**: Avalia condições e retorna diferentes valores com base no resultado.
  
```cds
    CASE
      WHEN Sales > 100 THEN 'High'
      ELSE 'Low'
    END
```
- **`COALESCE`**: Retorna o primeiro valor não nulo entre os argumentos fornecidos.
  
```cds
    COALESCE( NULL, 'SAP' ) -- Resultado: 'SAP'
```
- **`IF`**: Uma expressão condicional simples para retornar valores com base em uma condição.
  
```cds
    IF( Sales > 100, 'High', 'Low' )
```

---

## 📊 **Funções de Agregação**
Essas funções são usadas principalmente em operações de agrupamento dentro de views CDS:

- **`SUM`**: Soma os valores de uma coluna.
- **`AVG`**: Calcula a média dos valores de uma coluna.
- **`COUNT`**: Conta o número de registros.
- **`MIN`**: Retorna o menor valor de uma coluna.
- **`MAX`**: Retorna o maior valor de uma coluna.

---

## 🌟 **Exemplo de Uso em uma CDS View**
Aqui está um exemplo prático de como usar essas funções em uma CDS View:


```cds
@AbapCatalog.sqlViewName: 'ZMY_VIEW'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS com Scalar Functions'
define view ZMY_CDS_FUNCTIONS as select from sales_data {
    key sales_id,
    customer_id,
    UPPER(customer_name) as customer_upper,
    ADD_DAYS(sales_date, 7) as next_followup_date,
    ROUND(sales_amount, 2) as rounded_sales
}
```

### 🎛 Funções de Conversão

Essas funções são utilizadas para converter valores entre diferentes tipos de dados em CDS Views.

### 1. **`CAST`**
Converte valores para o tipo de dado especificado.

- **Sintaxe**: `CAST( <expression> AS <data_type> )`

  
  ```cds
  CAST( '123.45' AS DECIMAL(10,2) ) -- Converte string para decimal
  ```

---

### 2. **`TO_INTEGER`**
Converte para inteiro.

- **Sintaxe**: `TO_INTEGER( <expression> )` 

  
  ```cds
  TO_INTEGER( '123' ) -- Resultado: 123
  ```

---

### 3. **`TO_DECIMAL`**
Converte para decimal.

- **Sintaxe**: `TO_DECIMAL( <expression>, <precision>, <scale> )` 

  
  ```cds
  TO_DECIMAL( '123.456', 10, 2 ) -- Resultado: 123.46
  ```

---

### 4. **`TO_DOUBLE`**
Converte para ponto flutuante (número decimal com maior precisão).

- **Sintaxe**: `TO_DOUBLE( <expression> )`

  
  ```cds
  TO_DOUBLE( '123.456' ) -- Resultado: 123.456
  ```

---

### 5. **`TO_STRING`**
Converte qualquer valor para string.

- **Sintaxe**: `TO_STRING( <expression> )`

  
  ```cds
  TO_STRING( 123 ) -- Resultado: '123'
  ```

---

### 6. **`TO_DATE`**
Converte uma string ou número em uma data válida.

- **Sintaxe**: `TO_DATE( <expression> )`

  
  ```cds
  TO_DATE( '2024-12-25' ) -- Resultado: 25.12.2024
  ```

---

### 7. **`TO_TIME`**
Converte uma string ou número em um horário válido.

- **Sintaxe**: `TO_TIME( <expression> )`  

  
  ```cds
  TO_TIME( '14:30:00' ) -- Resultado: 14:30:00
  ```

---

### 8. **`TO_TIMESTAMP`**
Converte uma string ou número em um timestamp válido (data e hora combinadas).

- **Sintaxe**: `TO_TIMESTAMP( <expression> )`

  
  ```cds
  TO_TIMESTAMP( '2024-12-25 14:30:00' )
  ```

---

## 🌟 Exemplo de Uso com Funções de Conversão em CDS

```cds
@AbapCatalog.sqlViewName: 'ZMY_CONVERSION'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS com Funções de Conversão'
define view ZMY_CDS_CONVERSION as select from sales_data {
    key sales_id,
    customer_id,
    CAST(sales_amount AS DECIMAL(10,2)) as sales_as_decimal,
    TO_STRING(sales_amount) as sales_as_text,
    TO_DATE(sales_date) as sales_date_converted,
    TO_TIMESTAMP(CONCAT(sales_date, ' 12:00:00')) as timestamp
}
