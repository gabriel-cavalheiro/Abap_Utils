
# Comandos e Transações SAP

## Principais Transações

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

## Marretar Tabela

Na tela que se abre (ABAP debugger), insira as variáveis `GD-EDIT` e `GD-SAPEDIT` e atribua a elas o valor “X”.
- **SE16N_CD_KEY** - Rastrear quem marretou
- **SE16N_CD_DATA** – Rastrear o que foi marretado

## Outras Transações e Funções

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
- **SE80** - É usado para montar mensagens que serão utilizadas nos programas.
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
