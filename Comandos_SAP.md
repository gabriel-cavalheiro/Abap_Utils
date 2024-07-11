
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

- **AL11** – Diretórios / Pastas do Servidor do SAP.
- **ST05** – SQL Trace / Análise de Performance do Banco de Dados / Trace de Performance
- **SE30** – Análise do Tempo de Execução de Programas/Funções/Transações, para análises de Performance. Nas novas versões do SAP, utilizar a transação SAT
- **SE83** – Biblioteca de Reuso do SAP, contém diversos exemplos de programas e técnicas de programação
- **SPAD** – Administração de Spool, configuração de impressoras, drivers e formatos de página
- **SM12** – Remover bloqueios de objetos por usuário, tabela, argumento. Desbloquear objeto.
- **SM04** – Lista de usuários logados no sistema, com listagem das sessões abertas
- **SM50** – Lista com os de Processos em execução, debugar em background
- **SM51** – Lista de Servidores SAP
- **SE71** – Transação criar e dar manutenção em SapScript
- **SE78** – Administração de Gráficos, Gráficos de Formulários
- **SE73** – Administração de Fontes (útil para Código de Barras)
- **SE03** – Ferramentas para Transportes: Busca de objetos em requests, modificações, análise, inclusão de objetos, etc
- **SE18 / SE19** – Criação de BADIs
- **BAPI** – Busca por BAPIs de acordo com a área de aplicação
- **SNOTE** – Assistente de aplicação de Notas da SAP
- **SM37** – Seleção e visualização de Jobs no sistema
- **SM36** – Criação de Jobs
- **CG3Y** – Download de arquivos do servidor (sem precisar explorar as pastas)
- **CG3Z** – Upload de arquivos no servidor (sem precisar explorar as pastas)
- **SE14** – Atualização de definições de tabelas direto no banco de dados (use com MUITO CUIDADO)
- **CRM_UI** – Iniciar WebClient CRM, abrir CRM no browser
- **SM31** – Criar visão de tabelas (para criar telas com aquelas hierarquias de configurações da SPRO)
- **SE43** – Criação de Menus p/ transações ou tela inicial do usuário
- **SU01 / SU02 / SU03** – Administração de Usuários
- **TJ30** – Tabela de Status do CRM
- **SE21** – Administração de Pacotes / Criar novos pacotes
- **SE24** – Criar Classes, Interfaces, Classes de Exceção, Orientação à Objeto
- **SM35** – Transação de Batch Input
- **WE05** – Lista de IDOCs, Busca por IDOCs processados, status
- **WE20** – Protocolo de Transmissão, Tipos de Parceiro
- **WE60** – Documentação de IDOCs
- **VOFM** – SD, Fórmulas, Condições, Requisitos, Código p/ Controle de Cópia
- **CMOD** – Administração de Projetos (EXITs)
- **SMOD** – Ampliações e componentes dos projetos (EXITs)
- **SP01** – Visualização de SPOOL
- **NACE** – Controle de mensagens para Impressão
- **FIBF** – BTEs, EXITs de FI
- **SICF** – Ativação de Serviços do SAP
- **PFTC** – Atualização de Tarefas (Workflow)
- **SBWP** – Inbox do SAP (mensagens/emails internos, Workflow)
- **SWU3** – Customizing Workflow Automático
- **SWIA** – Log Workflow
- **ABAPDOCU** – Documentação ABAP e Exemplos de Código
- **ST01** – System Trace: Trace de autorizações, chamadas RFC, Kernel, DB, Table buffer
- **SCC4** – Visualizar detalhes do client, incluindo o sistema lógico atribuído e se cria request
- **SCC1** – Client Copy by Transport Request: Transportar requests de customizing entre clients do mesmo ambiente
- **BD64** – Criar/Manter modelo de distribuição para IDoc
- **STMS** – Transport Management System: Gerenciamento de transporte, incluindo definição do domínio, de parâmetros TP e rotas de transporte
- **SCOT** – Administração do SAPconnect
- **OB96** – Altera o form chamado no programa (Sap Script)
- **SM69** – Atualizar comandos externos SO
- **AL08** – Usuários autenticados no ambiente
- **DB01** – Analisar locks exclusivos no banco
- **DB02** – Monitor para tabelas e índices
- **DB12** – Logs de backup DBA
- **DB16** – Exibir resultados da verificação BD
- **DB24** – Operações administrativas de BD
- **SICK** – Verificação da instalação
- **SM01** – Bloquear transações
- **SM13** – Administrar registros de atualização (upload)
- **SM21** – Avaliação online do log de sistema
- **SM37** – Síntese da seleção de jobs
- **SM50** – Síntese do processo operacional
- **SM66** – Overview dos processos em execução
- **SMICM** – Monitor ICM SMLG Análise do tempo de resposta em grupos de logon
- **SMQ1** – Monitor qRFC (fila de saída)
- **SMQ2** – Monitor qRFC (fila de entrada)
- **SOST** – SAPconnect ordens de envio
- **SPAD** – Administração de spool
- **SP12** – Administração TemSe (checagem de consistência)
- **ST02** – Buffers
- **ST04** – Monitor de performance BD
- **ST06** – Monitor do sistema operacional
- **ST22** – ABAP análise dump
- **STRUST** – Trust Manager
- **SXMB_ADMIN** – Integration Engine – Monitoração
- **RZ11** – Administração de perfil
- **RZ10** – Edição de profile PFCG Administração de perfil
- **SU01** – Criação de usuário
- **SU10** – Manutenção de usuário em massa
- **SE30** – SQL Trace / Análise de Performance do Banco de Dados / Trace de Performance

