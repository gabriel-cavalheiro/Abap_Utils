
# Comandos e Transações SAP

## Sites Úteis 🤝

<https://www.abapemos.com/>

<https://www.abapemos.com/2020/03/alv-simples-com-clsalvtable.html>

<https://saplearners.com/>

<https://software-heroes.com/en/new-abap-statements>

## Marretar Tabela 🔨🪓

Na tela que se abre (ABAP debugger), insira as variáveis `GD-EDIT` e `GD-SAPEDIT` e atribua a elas o valor “X”.
- **SE16N_CD_KEY** - Rastrear quem marretou
- **SE16N_CD_DATA** – Rastrear o que foi marretado

## Transportar Textos SO10 📃

Executar o programa **RSTXTRAN** na SE38 e atribuir a uma task.

## Trechos de Códigos Utéis

### Melhorar perfomarce do FOR ALL ENTRIES

%_HINTS DB6 '&prefer_join 0& &prefer_join_with_fda 1&'.

### Exemplo:

~~~
SELECT * INTO TABLE db6out FROM t100
BYPASSING BUFFER
FOR ALL ENTRIES IN db6
WHERE sprsl = db6-sprsl
AND arbgb = db6-arbgb
AND msgnr = db6-msgnr
%_HINTS DB6 '&prefer_join 1& &prefer_join_with_fda 0&'.
~~~

___

### Somar valores de uma coluna com REDUCE

### Exemplo 1:

~~~
TYPES: tty_ekpo TYPE ekpo.
SELECT * FROM ekpo 
  INTO TABLE @DATA(it_ekpo)
  UP TO 10 ROWS.

DATA(lt2_dados) = REDUCE tty_ekpo( INIT cline = VALUE tty_ekpo( )
                 FOR GROUPS <group_key> OF <g> IN it_ekpo GROUP BY ( ebeln = <g>-ebeln ebelp = <g>-ebelp )
                 NEXT cline = VALUE #( BASE cline ( ebeln = <group_key>-ebeln ebelp = <group_key>-ebelp
                                                    netwr = REDUCE netwr( INIT val TYPE netwr
                                                                          FOR wa IN
                                                                          FILTER #( it_ekpo WHERE ebeln = <group_key>-ebeln AND ebelp = <group_key>-ebelp )
                                                                          NEXT val = val + wa-netwr ) ) ) ).
~~~

### Exemplo 2:

~~~
TYPES: BEGIN OF ty_itemdata_collect.
               INCLUDE TYPE bapi_incinv_create_item.
               TYPES  matnr TYPE ekpo-matnr.
      TYPES: END OF ty_itemdata_collect,
      tt_itemdata_collect TYPE STANDARD TABLE OF ty_itemdata_collect WITH DEFAULT KEY.

DATA(lt_aux_collect) = VALUE tt_itemdata_collect(
        FOR GROUPS <group_key> OF <group> IN lt_aux GROUP BY ( po_number = <group>-po_number
                                                               matnr     = <group>-matnr  )
          LET coll_line = REDUCE #(
              INIT line TYPE ty_itemdata_collect FOR <member> IN GROUP <group_key>
              NEXT
                   line-invoice_doc_item        = <member>-invoice_doc_item + 1
                   line-po_number               = <member>-po_number
                   line-po_item                 = <member>-po_item
                   line-ref_doc                 = <member>-ref_doc
                   line-ref_doc_year            = <member>-ref_doc_year
                   line-ref_doc_it              = <member>-ref_doc_it
                   line-de_cre_ind              = <member>-de_cre_ind
                   line-tax_code                = <member>-tax_code
                   line-taxjurcode              = <member>-taxjurcode
                   line-item_amount             = line-item_amount + <member>-item_amount
                   line-quantity                = line-quantity + <member>-quantity
                   line-po_unit                 = <member>-po_unit
                   line-po_unit_iso             = <member>-po_unit_iso
                   line-po_pr_qnt               = line-po_pr_qnt + <member>-po_pr_qnt
                   line-po_pr_uom               = <member>-po_pr_uom
                   line-po_pr_uom_iso           = line-po_pr_uom_iso + <member>-po_pr_uom_iso
                   line-cond_type               = <member>-cond_type
                   line-cond_st_no              = <member>-cond_st_no
                   line-cond_count              = <member>-cond_count
                   line-sheet_no                = <member>-sheet_no
                   line-item_text               = <member>-item_text
                   line-final_inv               = <member>-final_inv
                   line-sheet_item              = <member>-sheet_item
                   line-grir_clear_srv          = <member>-grir_clear_srv
                   line-freight_ven             = <member>-freight_ven
                   line-cshdis_ind              = <member>-cshdis_ind
                   line-retention_docu_currency = line-retention_docu_currency + <member>-retention_docu_currency
                   line-retention_percentage    = line-retention_percentage + <member>-retention_percentage
                   line-retention_due_date      = <member>-retention_due_date
                   line-no_retention            = <member>-no_retention
                   line-inv_itm_origin          = <member>-inv_itm_origin
         ) IN ( coll_line ) ) .
~~~

___

### Contar caracteres

### Exemplo:

~~~
v_len = strlen( v_str ).
~~~

___

### Zeros à esquerda e à direita em ABAP

#### Com Function:
~~~
DATA(lv_order_number) = CONV aufnr( 12345 ).
WRITE |{ lv_order_number }|.    "Output: 12345

"Add leading zeros
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = lv_order_number
  IMPORTING
    output = lv_order_number.

WRITE |{ lv_order_number }|.    "Output: 000000012345

"Remove leading zeros
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    input  = lv_order_number
  IMPORTING
    output = lv_order_number.

WRITE |{ lv_order_number }|.    "Output: 12345
~~~

#### Short Form:

~~~~
DATA(lv_order_number) = CONV aufnr( 12345 ).
WRITE |{ lv_order_number }|.    "Output: 12345

lv_order_number =  |{ lv_order_number ALPHA = IN }|.
WRITE |{ lv_order_number }|.    "Output: 000000012345

lv_order_number =  |{ lv_order_number ALPHA = OUT }|.
WRITE |{ lv_order_number }|.    "Output: 12345
~~~~

#### Inline Declaration com operador CONV:

~~~~
DATA(lv_order_number) = CONV aufnr( |{ '12345' ALPHA = IN }| ).
WRITE |{ lv_order_number }|.    "Output: 000000012345
~~~~

#### Removendo com SHIFT:

~~~~
DATA(lv_order_number) = CONV aufnr( |{ '12345' ALPHA = IN }| ).
WRITE |{ lv_order_number }|.    "Output: 000000012345

SHIFT lv_order_number LEFT DELETING LEADING '0'.
WRITE |{ lv_order_number }|.    "Output: 12345
~~~~

#### Add Zeros com OVERLAY:

~~~~
DATA(lv_order_number) = CONV aufnr( 12345 ).
WRITE |{ lv_order_number }|.    "Output: 12345

"Variável auxiliar do tipo string, sem zeros à esquerda ou à direita
DATA(lv_order_number_string) =  |{ lv_order_number ALPHA = OUT }|.
OVERLAY lv_order_number_string WITH '000000000000'.
WRITE |{ lv_order_number_string }|.    "Output: 123450000000
~~~~

___

### Form para Authorith-Check
~~~
*&---------------------------------------------------------------------*
*& Form zf_auth
*&---------------------------------------------------------------------*
FORM zf_auth  USING  p_bukrs      TYPE bukrs
           CHANGING  p_continue
                     p_check_auth.

  CLEAR p_continue.

  CONSTANTS:
    c_actvt TYPE xufield     VALUE 'ACTVT',      " ACTVT
    c_bukrs TYPE xufield     VALUE 'BUKRS',      " BUKRS
    c_03    TYPE xuval       VALUE '03'.         " Act. leitura

  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
  ID c_actvt FIELD c_03
  ID c_bukrs FIELD p_bukrs.

  IF sy-subrc IS NOT INITIAL.
*   Sem autorização para uso da transação
    p_continue   = 'X'.
    p_check_auth = 'X'.
  ENDIF.

ENDFORM.
~~~~

___

### Principais Transações

- **SMICM** - IP do Client
- **SU53** - Log de objeto de autorização
- **ST22** - Log de Dumps
- **SE03** - Desbloqueio de objetos na request
- **RDDIT076** - Alterar status da request - ir pela SE38
- **/N/SAST/LOGON** - Solicitar usuário SAST
- **/n/iwfnd/maint_service** - Verificar funcionamento de API
- **SEGW** - Visualizar a definição do serviço "API"
- **SO10** - Impressora
- **ZPP035** - Impressora
- **RS_HDSYS_CALL_TC_VARIANT** - Authority check
- **Reset de senha SAP** - Senha: `pass`
- **/n/pgtpa/op_ger** - Op SOFICOM
- **Para voltar uma instrução** - Posicionar na linha e pressionar Shift + F12
- **SU01D** - Visualizar dados de usuário SAP
- **SOST** - Verificar status de envio de email
- **ABAPDOCU** – Documentação ABAP e Exemplos de Código (por Samuel Xavier)
- **AL08** – Usuários autenticados no ambiente
- **AL11** – Diretórios / Pastas do Servidor do SAP
- **BAPI** – Busca por BAPIs de acordo com a área de aplicação
- **BD64** – Criar/Manter modelo de distribuição para IDoc
- **CMOD** – Administração de Projetos ( EXITs )
- **CRM_UI** – Iniciar WebClient CRM, abrir CRM no browser
- **DB01** – Analisar locks exclusívos no banco
- **DB02** – Monitor para tabelas e índices
- **DB12** – Logs de backup DBA
- **DB16** – Exibir resultados da verificação BD
- **DB24** – Operações administrativas de BD
- **FIBF** – BTEs, EXITs de FI
- **ME23N** – Consulta pedido
- **NACE** – Controle de mensagens para Impressão
- **OB96** – Altera o form chamado no programa. (Sap Script) – por Bruno Nicolau
- **PFCG** – Dar autorização para um usuário
- **PFTC** – Atualização de Tarefas ( Workflow )
- **RZ10** – Edição de profile PFCG Administração de perfil
- **RZ11** – Administração de perfil
- **SCC1** – Client Copy by Transport Request: Transportar requests de customizing entre clients do mesmo ambiente
- **SCC4** – Visualizar detalhes do client, incluindo o sistema lógico atribuído e se cria request
- **SCOT** – Administração do SAPconnect
- **SE03** – Ferramentas para Transportes: Busca de objetos em requests, modificações, análise, inclusão de objetos, etc
- **SE10** – Consulta request
- **SE11** – Criar tabelas e consultar variaveis sys
- **SE14** – Atualização de definições de tabelas direto no banco de dados
- **SE18** – Visualizar BADI
- **SE19** – Implementação de BADI
- **SE21** – Administração de Pacotes / Criar novos pacotes
- **SE24** – Criar Classes, Interfaces, Classes de Exceção, Orientação à Objeto
- **SE30** – Análise do Tempo de Execução de Programas/Funções/Transações, para análises de Performance. Nas novas versões do SAP, utilizar a transação SAT
- **SE37** – Módulo de função
- **SE43** – Criação de Menus p/ transações ou tela inicial do usuário
- **SE71** – Transação criar e dar manutenção em SapScript
- **SE78** – Administração de Gráficos, Gráficos de Formulários e upload de imagens
- **SE80** – Pacotes
- **SE91** - É usado para montar mensagens que serão utilizadas nos programas.
- **SE93** – Criar transação
- **SHDB** – Batch input
- **SICK** – Verificação da instalação
- **SICF** – Ativação de Serviços do SAP
- **SM01** – Bloquear transações
- **SM04** – Lista de sessões de usuários / Liberar usuário de transação travada
- **SM12** – Remover bloqueios de objetos por usuário, tabela, argumento. Desbloquear objeto
- **SM13** – Administrar registros de atualização (upload)
- **SM21** – Avaliação online do log de sistema
- **SM30** – Visão tabelas manutenção
- **SM31** – Criar visão de tabelas (para criar telas com aquelas hierarquias de configurações da SPRO)
- **SM35** – Transação de Batch Input
- **SM37** – Seleção e visualização de Jobs no sistema
- **SM50** – Lista com os de Processos em execução, debuggar em background
- **SM51** – Lista de Servidores SAP
- **SM66** – Overview dos processos em execução
- **SMICM** – Monitor ICM SMLG Análise do tempo de resposta em grupos de logon
- **SMQ1** – Monitor qRFC (fila de saída)
- **SMQ2** – Monitor qRFC (fila de entrada)
- **SNOTE** – Assistente de aplicação de Notas da SAP
- **SP01** – Controle de saída (spool)
- **SP12** – Administração TemSe (checagem de consistência)
- **SPAD** – Administração de Spool, configuração de impressoras, drivers e formatos de página
- **ST01** – System Trace: Trace de autorizações, chamadas RFC, Kernel, DB, Table buffer
- **ST02** – Buffers
- **ST04** – Monitor de performance BD
- **ST05** – SQL Trace / Análise de Performance do Banco de Dados / Trace de Performance
- **ST06** – Monitor do sistema operacional
- **ST22** – ABAP análise dump
- **STMS** – Transport Management System: Gerenciamento de transporte, incluindo definição do domínio, de parâmetros TP e rotas de transporte
- **STRUST** – Trust Manager
- **SU01** – Criação de usuário / Funções do usuário
- **SU02** – Administração de Usuários
- **SU03** – Administração de Usuários
- **SU10** – Manutenção de usuário em massa
- **SU21** – Criar Classe e Obj de autorização
- **SU53** – Exibir verificação de autorização
- **SWIA** – Log Workflow
- **SWU3** – Customizing Workflow Automático
- **SXMB_ADMIN** – Integration Engine – Monitoração
- **SOST** – SAPconnect ordens de envio
- **TJ30** – Tabela de Status do CRM
- **VOFM** – SD, Fórmulas, Condições, Requisitos, Código p/ Controle de Cópia
- **WE05** – Lista de IDOCs, Busca por IDOCs processados, status
- **WE20** – Protocolo de Transmissão, Tipos de Parceiro
- **WE60** – Documentação de IDOCs
