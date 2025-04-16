# Criando objetos RAP com gerador ADT integrado no Eclipse

<a>https://medium.com/@vitorkazuma/criando-crud-rap-com-gerador-adt-integrado-em-poucos-cliques-2396925d69eb<a/>

# Comparação de Sintaxes: EML no RAP

Abaixo está uma tabela completa comparando as principais operações no **ABAP tradicional** e sua contraparte moderna no **RAP usando EML**.

| **Operação**                  | **ABAP Tradicional**                            | **EML no RAP**                                           |
|-------------------------------|------------------------------------------------|--------------------------------------------------------|
| **Seleção de dados**          | `SELECT * FROM tabela INTO ...`                 | `CALL ENTITY entidade QUERY RESULTS ...`              |
| **Inserção de dados**         | `INSERT INTO tabela VALUES ...`                 | `CALL ENTITY entidade CREATE ENTITY TABLE ...`         |
| **Atualização de dados**      | `UPDATE tabela SET campo = valor ...`           | `CALL ENTITY entidade UPDATE ENTITY TABLE ...`         |
| **Exclusão de dados**         | `DELETE FROM tabela WHERE ...`                  | `CALL ENTITY entidade DELETE ENTITY TABLE ...`         |
| **Commit de transação**       | `COMMIT WORK.`                                  | **Não necessário (gerenciado pelo RAP automaticamente)** |
| **Rollback de transação**     | `ROLLBACK WORK.`                                | **Não necessário (gerenciado pelo RAP automaticamente)** |
| **Verificação de autorizações**| `AUTHORITY-CHECK OBJECT ...`                    | Configurado em CDS com anotações, como `@AccessControl.authorizationCheck: #CHECK` |
| **Validações de dados**       | Programadas manualmente no código               | Declaradas nas entidades BO e CDS                     |
| **Mensagens de erro**         | `RAISE EXCEPTION` ou `MESSAGE`                  | Tratadas com `FAILED DATA` no EML                     |
| **Filtro de dados**           | `SELECT ... WHERE campo = valor`                | `CALL ENTITY entidade QUERY WHERE ...`                |
| **Ordenação de dados**        | `SELECT ... ORDER BY campo`                     | `CALL ENTITY entidade QUERY ORDER BY ...`             |
| **Paginação**                 | Controlada manualmente com `UP TO ... ROWS`     | Declarada com `PAGING` no EML                         |
| **Execução de ações**         | Programação manual em métodos                   | `CALL ACTION` no RAP                                   |
| **Consumo de dados em UI**    | Requer configuração manual de serviços OData   | Gerado automaticamente a partir do CDS e BO           |
| **Controle de travas**        | Programado com `ENQUEUE` e `DEQUEUE`            | Configurado no BO com comportamentos específicos       |
| **Controle de eventos**       | Programação manual em user exits ou BAdIs       | Definido em comportamentos (behavior definitions)      |

---

## Exemplos de Sintaxes Modernas no RAP com EML

### 1. **Seleção de dados**
```abap
CALL ENTITY ZI_SALES_ORDER QUERY
  RESULTS DATA(lt_sales_order)
  WHERE SalesOrderType = 'Standard'.
```

### 2. **Inserção de dados**
```abap
DATA: lt_sales_order TYPE TABLE FOR CREATE ZI_SALES_ORDER.

APPEND VALUE #( SalesOrderId = '500000001'
                SalesOrderType = 'Standard'
                BuyerName      = 'John Doe' ) TO lt_sales_order.

CALL ENTITY ZI_SALES_ORDER CREATE
  ENTITY TABLE lt_sales_order
  FAILED DATA lt_failed
  REPORTED DATA lt_reported.
```

### 3. **Atualização de dados**
```abap
DATA: lt_sales_order_update TYPE TABLE FOR UPDATE ZI_SALES_ORDER.

APPEND VALUE #( SalesOrderId = '500000001'
                BuyerName    = 'Jane Doe' ) TO lt_sales_order_update.

CALL ENTITY ZI_SALES_ORDER UPDATE
  ENTITY TABLE lt_sales_order_update
  FAILED DATA lt_failed.
```

### 4. **Exclusão de dados**
```abap
DATA: lt_sales_order_delete TYPE TABLE FOR DELETE ZI_SALES_ORDER.

APPEND VALUE #( SalesOrderId = '500000001' ) TO lt_sales_order_delete.

CALL ENTITY ZI_SALES_ORDER DELETE
  ENTITY TABLE lt_sales_order_delete
  FAILED DATA lt_failed.
```

### 5. **Tratamento de Erros**
```abap
CALL ENTITY ZI_SALES_ORDER DELETE
  ENTITY TABLE lt_sales_order
  FAILED DATA lt_failed.

IF lt_failed IS NOT INITIAL.
  LOOP AT lt_failed INTO DATA(ls_failed).
    " Tratar o erro
    WRITE: / ls_failed-%error_code, ls_failed-%error_message.
  ENDLOOP.
ENDIF.

```

# Uso do Comando `READ ENTITIES` no RAP

O comando **`READ ENTITIES`** no RAP (RESTful ABAP Programming Model) é utilizado para ler dados de **entidades principais** ou **associações relacionadas** definidas nas CDS e Behavior Definitions (BDEF).

---

## 🛠️ **Passo a Passo para Utilizar o `READ ENTITIES`**

### 1. **Definir Associações na CDS**
Para utilizar o `READ ENTITIES`, a CDS deve possuir associações configuradas com `@ObjectModel.association`.

#### Exemplo de Associação:
```abap
@ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
define view ZI_Sales_Order as select from zsales_order
association [0..*] to ZI_Sales_Order_Item as _Items on $projection.SalesOrderId = _Items.SalesOrderId
{
    key SalesOrderId,
    SalesOrderType,
    BuyerName,
    _Items
}
```

---

### 2. **Configurar o Behavior Definition (BDEF)**
O comportamento da entidade principal e das associações deve ser configurado no arquivo `.bdef`.

#### Exemplo de Behavior Definition:
```abap
managed implementation in class zbp_sales_order unique;
define behavior for ZI_Sales_Order
persistent table zsales_order
{
    field (readonly) SalesOrderId, SalesOrderType, BuyerName;

    association _Items { create; update; delete; }

    create;
    update;
    delete;
}

define behavior for ZI_Sales_Order_Item
persistent table zsales_order_item
{
    field (readonly) SalesOrderId, ItemPosition, ProductId, Quantity;

    create;
    update;
    delete;
}
```

---

## 🖥️ **Exemplos Práticos**

### Exemplo 1: Ler Dados de uma Entidade Principal

```abap
DATA: lt_sales_order TYPE TABLE FOR READ ZI_Sales_Order,
      lt_failed      TYPE TABLE FOR FAILED ZI_Sales_Order.

" Ler dados da entidade principal ZI_Sales_Order
READ ENTITIES OF ZI_Sales_Order
  ENTITY SalesOrder
  FIELDS ( SalesOrderId SalesOrderType BuyerName )  " Campos desejados
  WITH VALUE #( ( SalesOrderId = '500000001' ) )    " Condição
  RESULT DATA(lt_sales_order)
  FAILED DATA(lt_failed).

LOOP AT lt_sales_order INTO DATA(ls_order).
  WRITE: / ls_order-SalesOrderId, ls_order-BuyerName.
ENDLOOP.
```

---

### Exemplo 2: Ler Dados de uma Associação
Se a entidade possui associações definidas, é possível buscar os dados relacionados diretamente.

```abap
DATA: lt_items TYPE TABLE FOR READ ZI_Sales_Order_Item,
      lt_failed TYPE TABLE FOR FAILED ZI_Sales_Order_Item.

" Ler itens relacionados à ordem de venda 500000001
READ ENTITIES OF ZI_Sales_Order
  ENTITY _Items                         " Associação _Items
  FIELDS ( SalesOrderId ItemPosition ProductId Quantity )
  WITH VALUE #( ( SalesOrderId = '500000001' ) )  " Filtro na entidade principal
  RESULT DATA(lt_items)
  FAILED DATA(lt_failed).

LOOP AT lt_items INTO DATA(ls_item).
  WRITE: / ls_item-SalesOrderId, ls_item-ItemPosition, ls_item-ProductId, ls_item-Quantity.
ENDLOOP.
```

---

## 🔑 **Quando Usar `READ ENTITIES`**

| **Cenário**                     | **Descrição**                                                                 |
|----------------------------------|-------------------------------------------------------------------------------|
| **Leitura de entidade principal** | Usado para buscar dados de uma entidade BO principal.                        |
| **Leitura de associações**       | Ideal para buscar dados relacionados de associações configuradas na CDS.      |
| **Navegação entre entidades**    | Permite navegar por associações e buscar dados de subentidades estruturadas. |

---

## 💡 **Diferença entre `QUERY` e `READ ENTITIES`**

| **Comando**              | **Finalidade**                                                                                     |
|---------------------------|---------------------------------------------------------------------------------------------------|
| **`CALL ENTITY ... QUERY`** | Consulta direta a uma entidade principal. Filtra, ordena e pagina dados de forma genérica.         |
| **`READ ENTITIES`**        | Operação de leitura de entidades relacionadas ou subentidades (associações).                     |

---

# Derived Types no RAP (RESTful ABAP Programming Model)

Os **Derived Types** no RAP referem-se a tipos especiais com o prefixo `%`, como `%key`, `%cid`, `%tky`, entre outros. Eles ajudam a identificar chaves principais, chaves técnicas, estados de rascunho e a controlar campos no contexto de entidades e associações no RAP.

---

## 🛠️ **Principais Derived Types no RAP**

| **Derived Type** | **Descrição**                                                                                     |
|-------------------|-------------------------------------------------------------------------------------------------|
| `%key`           | Representa a chave principal da entidade, definida diretamente na CDS.                         |
| `%cid`           | Identificador transitório usado para referenciar registros temporários ao criar entidades.      |
| `%tky`           | Chave técnica usada para navegação em associações e identificação interna de registros.         |
| `%is_draft`      | Indica se uma entidade é um rascunho (Draft).                                                   |
| `%control`       | Define regras e permissões para campos durante operações como criação ou atualização.           |
| `%parent_key`    | Usado para identificar a chave da entidade pai em associações (relação entre pai e filho).       |

---

## 📜 **Descrição dos Tipos em Detalhe**

### 1. `%key`
- **O que é:** Refere-se à chave principal da entidade, definida como `key` na CDS.
- **Quando usar:** Sempre que precisar acessar ou identificar um registro específico na tabela principal da entidade.

#### Exemplo:
```abap
define root view entity ZI_Sales_Order
  key SalesOrderId : abap.char(10)
{
  SalesOrderId,
  BuyerName,
  SalesOrderType
}
```
Neste caso, o campo `SalesOrderId` será tratado como `%key` nas operações com EML.

---

### 2. `%cid`
- **O que é:** Um identificador transitório usado principalmente ao criar registros. Ele permite que você associe dados temporários aos registros que estão sendo criados.
- **Quando usar:** Durante operações de criação (`CREATE`) em que identificadores permanentes ainda não estão disponíveis.

#### Exemplo de Uso com `%cid`:
```abap
DATA lt_failed TYPE TABLE FOR FAILED ZI_Sales_Order.

MODIFY ENTITIES OF ZI_Sales_Order
  ENTITY SalesOrder
  CREATE FIELDS ( SalesOrderId BuyerName )
  WITH VALUE #( ( %cid = '1' SalesOrderId = '500001' BuyerName = 'Cliente A' ) )
  FAILED DATA(lt_failed).
```
Aqui, `%cid` vincula um identificador transitório ao registro criado.

---

### 3. `%tky`
- **O que é:** Representa uma chave técnica, usada para identificar registros de forma única, especialmente em associações.
- **Quando usar:** Em operações que envolvem navegação entre entidades relacionadas.

#### Exemplo de Navegação:
```abap
READ ENTITIES OF ZI_Sales_Order
  ENTITY _Items
  FIELDS ( ProductId Quantity )
  WITH VALUE #( ( %tky = 'GUID_OR_TK' ) )
  RESULT DATA(lt_items).
```
Neste exemplo, `%tky` identifica o registro pai ao navegar para os itens associados.

---

### 4. `%is_draft`
- **O que é:** Indica se a entidade é um rascunho.
- **Quando usar:** Quando for necessário verificar ou manipular o estado de rascunho de uma entidade.

#### Exemplo de Uso:
```abap
READ ENTITIES OF ZI_Sales_Order
  ENTITY SalesOrder
  FIELDS ( SalesOrderId %is_draft )
  RESULT DATA(lt_sales_order).

LOOP AT lt_sales_order INTO DATA(ls_order).
  IF ls_order-%is_draft = abap_true.
    WRITE: / 'Rascunho encontrado:', ls_order-SalesOrderId.
  ENDIF.
ENDLOOP.
```

---

### 5. `%control`
- **O que é:** Controla quais campos podem ser manipulados durante operações de criação ou atualização.
- **Quando usar:** Em operações que exigem controle granular sobre quais campos podem ser alterados.

#### Exemplo de Controle de Campos:
```abap
MODIFY ENTITIES OF ZI_Sales_Order
  ENTITY SalesOrder
  UPDATE FIELDS ( BuyerName )
  WITH VALUE #( ( SalesOrderId = '500001' BuyerName = 'Cliente B' %control-BuyerName = 'U' ) ).
```
Aqui, `%control-BuyerName = 'U'` indica que o campo `BuyerName` pode ser atualizado.

---

### 6. `%parent_key`
- **O que é:** Identifica a chave da entidade pai em associações, permitindo a navegação em hierarquias de dados.
- **Quando usar:** Sempre que precisar acessar os dados do registro pai relacionado a uma entidade filha.

#### Exemplo:
```abap
READ ENTITIES OF ZI_Sales_Order
  ENTITY _Items
  FIELDS ( %parent_key ProductId )
  RESULT DATA(lt_items).

LOOP AT lt_items INTO DATA(ls_item).
  WRITE: / 'Pedido Pai:', ls_item-%parent_key.
ENDLOOP.
```

---

## 🖥️ **Resumo das Operações com Derived Types no RAP**

| **Operação**                 | **Descrição**                                                                               |
|------------------------------|-------------------------------------------------------------------------------------------|
| **Criar registros**          | Uso de `%cid` para mapear identificadores transitórios aos registros criados.              |
| **Ler associações**          | Uso de `%tky` para navegar entre entidades relacionadas.                                   |
| **Manipular rascunhos**      | Uso de `%is_draft` para verificar se uma entidade está no estado de rascunho.              |
| **Controle de campos**       | Uso de `%control` para definir campos que podem ser criados ou atualizados dinamicamente.  |
| **Navegar hierarquias**      | Uso de `%parent_key` para acessar chaves de entidades pai em associações.                  |

---

