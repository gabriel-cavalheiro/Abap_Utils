# 🚀 Expondo BAPI com RAP Facade  
**BAPI no CRUD de App Gerenciado**  

---

### 📖 Introdução  
Vamos mostrar uma opção de como chamar **BAPIs** dentro do RAP utilizando o próprio CRUD de um App Managed.

---

### 🗃️ Fonte de Dados  
Vamos ver a tabela Z que vai derivar nossas CDSs:

```abap
@EndUserText.label : 'FI'  
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE  
@AbapCatalog.tableCategory : #TRANSPARENT  
@AbapCatalog.deliveryClass : #A  
@AbapCatalog.dataMaintenance : #ALLOWED  
define table ypoc_fi_doc {  
  key mandt            : mandt not null;  
  key documento        : belnr_d not null;  
  key exercicio        : gjahr not null;  
  key linha            : buzei not null;  
      data_lancamento  : abap.dats;  
}
```
### 📜CDS Root - Composite
Após termos nossa tabela, vamos criar nossa CDS que vai ser a base do nosso App:

```abap
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Teste'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity YR_POC_FI_DOC
  as select from ypoc_fi_doc
{
  key documento       as Documento,
  key exercicio       as Exercicio,
  key linha           as Linha,
      data_lancamento as DataLancamento
}
```

### 🛠️ CDS Root - Consumption
Com a nossa base criada, vamos agora criar nossa Consumption que vai expor as annotations de UI:

```abap
@EndUserText.label: 'Teste'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity YC_POC_FI_DOC
  as projection on YR_POC_FI_DOC
{
      @UI.facet: [
      { 
        id: 'Projeto', 
        purpose: #STANDARD, 
        type: #IDENTIFICATION_REFERENCE, 
        label: 'Detalhes', 
        position: 10 }
      ]

      @UI.lineItem: [{ position: 10 }]
      @UI.identification: [{ position: 10 }]
      @EndUserText.label: 'Documento'
  key Documento,
      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 20 }]
      @EndUserText.label: 'Exercício'
  key Exercicio,
      @UI.lineItem: [{ position: 30 }]
      @UI.identification: [{ position: 30 }]
      @EndUserText.label: 'Linha'
  key Linha,
      @UI.lineItem: [{ position: 40 }]
      @UI.identification: [{ position: 40 }]
      @EndUserText.label: 'Data Lançamento'
      DataLancamento
}
```
### `Behavior da Root`
Após ter as duas CDSs criadas, vamos criar nosso Behavior da primeira CDS, nele que vamos ter as implementações do CRUD.

```abap
managed implementation in class zbp_r_poc_fi_doc unique;
strict ( 2 );

define behavior for YR_POC_FI_DOC //alias <alias_name>
with unmanaged save
late numbering
lock master
authorization master ( instance )
//etag master <field_name>
{
  create;
  update;
  delete;
}
```
*OBS:* Adicionamos ai a tag WITH UNMANAGED SAVE que vai servir para desabilitar o SAVE do nosso App, já que ele é Managed (A SAP controla o Save Sequence).
Com isso vamos ter a possibilidade de implementar o que queremos no SAVE.

### `Implementação da Classe (zbp_r_poc_fi_doc) do Behavior`
Ao utilizar o assistente do ADT, sua classe vai ser criada automáticamente.

Aqui vai um exemplo de como ficaria a implementação para podermos chamar a BAPI dentro do CRUD do App

```abap
CLASS lsc_yr_poc_fi_doc DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.
    METHODS save_modified REDEFINITION.
ENDCLASS.

CLASS lsc_yr_poc_fi_doc IMPLEMENTATION.

  METHOD adjust_numbers.

    " Vamos receber o que o usuário está salvando!
    READ ENTITY IN LOCAL MODE yr_poc_fi_doc
     ALL FIELDS WITH VALUE #( FOR ls_line IN mapped-yr_poc_fi_doc ( %pky = ls_line-%pre ) )
              RESULT DATA(lt_result).

    " Exemplo de uma chamada de BAPI
    DATA: ls_flight TYPE bapisfldat.
    CALL FUNCTION 'BAPI_FLIGHT_GETDETAIL'
      EXPORTING
        airlineid    = CONV s_carr_id( 'AA' )
        connectionid = CONV s_conn_id( '17' )
        flightdate   = CONV dats( '20231005' )
      IMPORTING
        flight_data  = ls_flight.

  ENDMETHOD.

  METHOD save_modified.
    " Só implementar quando quiser salvar o que o usuário está criando!
  ENDMETHOD.

ENDCLASS.


CLASS lhc_yr_poc_fi_doc DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR yr_poc_fi_doc RESULT result.

ENDCLASS.

CLASS lhc_yr_poc_fi_doc IMPLEMENTATION.
  METHOD get_instance_authorizations.
  ENDMETHOD.
ENDCLASS.
```
### `Behavior - Consumption`
Após criar o Behavior da nossa Root, devemos criar o Behavior da projeção.

```abap
projection;
strict ( 2 );

define behavior for YC_POC_FI_DOC //alias <alias_name>
{
  use create;
  use update;
  use delete;
}
```
### `Publicação`
Agora que tudo está montado, podemos criar os Services Definition e Binding.

```abap
@EndUserText.label: 'Teste'
define service YUI_POC_FI_DOCUMENTO {
  expose YC_POC_FI_DOC;
}
```
![image](https://github.com/user-attachments/assets/50f3a277-879d-4fd4-908c-00ae9ffe7a88)

*Feito!*
Agora você consegue colocar o BREAK-POINT no método *ADJUST_NUMBERS*, a tabela *LT_RESULT* vai ter os dados que precisa para poder preencher a sua BAPI.
