# Validação dos Ajustes Realizados - ZGLMM436

Este documento descreve a validação conceitual dos ajustes implementados no programa ABAP `ZGLMM436` (arquivo `ZGLMM436_revisado.txt`), com base na especificação funcional `EF-MM-ZGLMM436 ajuste lancamento AVD 1.docx` e no documento de pontos de ajuste.

Como a execução direta do código ABAP não é possível neste ambiente, a validação é realizada através da análise dos cenários de teste e dos resultados esperados para cada modificação principal.

## Cenários de Validação por Ponto de Ajuste:

### 1. Ponto 1: Alteração no SELECT (Busca de Pedidos com base no parâmetro WERKS da ZGLBC068)
   - **Ajuste Implementado:** Adicionada leitura do parâmetro `WERKS` da `ZGLBC068` para o range `r_werks_param` no `FORM zf_busca_parametros`. A lógica de seleção principal (ainda a ser identificada e ajustada no `FORM zf_processamento` ou similar) deve usar este range para decidir se comenta ou não `AND status_receb = c_s`.
   - **Cenário 1.1:** Parâmetro `WERKS` na `ZGLBC068` contém o centro '0001'. Usuário executa ZGLMM436 com `S_WERKS` (tela) = '0001'.
     - **Resultado Esperado:** O `SELECT` principal de pedidos (ex: em `PERFORM zf_seleciona_dados_miro`) deve ser executado *sem* a condição `AND cb~status_receb = c_s` (ou equivalente), pois `s_werks-low IN r_werks_param` seria verdadeiro.
   - **Cenário 1.2:** Parâmetro `WERKS` na `ZGLBC068` contém '0001'. Usuário executa ZGLMM436 com `S_WERKS` = '0002'.
     - **Resultado Esperado:** O `SELECT` principal deve ser executado *com* a condição `AND cb~status_receb = c_s`, pois `s_werks-low IN r_werks_param` seria falso.
   - **Cenário 1.3:** Parâmetro `WERKS` não configurado na `ZGLBC068`. Usuário executa ZGLMM436 com `S_WERKS` = '0001'.
     - **Resultado Esperado:** O `SELECT` principal deve ser executado *com* a condição `AND cb~status_receb = c_s`, pois `r_werks_param` estaria vazio.
   - **Observação:** A implementação exata da condicional no SELECT principal ainda requer que o desenvolvedor ABAP identifique o local correto e aplique a lógica `IF s_werks IN r_werks_param... ELSE... ENDIF` em torno das cláusulas WHERE relevantes.

### 2. Ponto 2: Verificação de MIRO sem MIGO na Tela Inicial
   - **Ajuste Implementado:** A especificação é um pouco vaga ("IF sinalizado em vermelho", "verificar se p_ <> ‘N’"). Não foi possível identificar um parâmetro de tela `p_` claro no código original para esta funcionalidade específica. Esta lógica precisaria ser inserida no fluxo principal de processamento de cada item, antes da chamada da BAPI da MIRO. A validação da existência da MIGO (`PERFORM zf_verifica_migo` no código original) já existe. A nova condição `p_` (se for um novo parâmetro de tela ou configuração) precisaria ser integrada.
   - **Cenário 2.1 (Conceitual):** Se um parâmetro `p_check_obrigatoriedade_migo` (exemplo) for 'S' e a MIGO não existir para o item.
     - **Resultado Esperado:** Mensagem de erro apropriada. Lançamento da MIRO para o item é pulado.
   - **Cenário 2.2 (Conceitual):** Se `p_check_obrigatoriedade_migo` for 'N'.
     - **Resultado Esperado:** Lançamento da MIRO prossegue, mesmo sem MIGO (se outras condições permitirem).

### 3. Ponto 3: Validação de Moeda no Lançamento da MIRO (FORM ZF_EXECUTA_MIRO)
   - **Ajuste Implementado:** No `FORM zf_executa_miro` (ou onde `w_ekko-waers` é preenchido antes da BAPI), uma verificação `IF w_ekko-waers <> 'BRL'` seria adicionada.
   - **Cenário 3.1:** Pedido com `w_ekko-waers = 'USD'`. 
     - **Resultado Esperado:** Mensagem "Lançamento bloqueado, moeda USD incorreta" no log. `CONTINUE` para o próximo item do loop principal.
   - **Cenário 3.2:** Pedido com `w_ekko-waers = 'BRL'`. 
     - **Resultado Esperado:** Processamento continua.
   - **Observação:** Este ajuste deve ser inserido pelo desenvolvedor no ponto correto do `FORM zf_executa_miro` ou equivalente.

### 4. Ponto 4: Novo Botão "Inclusão de boleto no faturamento" e FORM ZF_EXECUTA_FB09
   - **Ajuste Implementado:** 
     - Novo `SELECTION-SCREEN FUNCTION KEY 2` e texto `sscrfields-functxt_02` no `INITIALIZATION`.
     - `CASE sscrfields-ucomm WHEN 'FC02'` no `AT SELECTION-SCREEN` chama `PERFORM zf_display_boleto_screen`.
     - `FORM zf_display_boleto_screen` (placeholder para tela de entrada) chama `FORM zf_executa_fb09`.
     - `FORM zf_executa_fb09` implementado com validações (BSEG, BKPF-AWKEY, ZTBMM_NF_FORN_CB) e chamada BDC para FB09 (via `FORM zf_fill_bdc_fb09`).
     - Lógica para chamada pós-MIRO (dentro de `zf_executa_miro`) para atualizar o documento FI via FB09.
   - **Cenário 4.1 (Botão e Tela):** Usuário clica no botão "Inclusão de boleto no faturamento" (FC02).
     - **Resultado Esperado:** `PERFORM zf_display_boleto_screen` é chamado. A tela de entrada de boletos (a ser desenvolvida) deve ser exibida. Após entrada e confirmação, `zf_executa_fb09` é chamado.
   - **Cenário 4.2 (Processamento FB09 - Sucesso):** Dados válidos na tela de boletos. Doc. contábil existe, AWKEY encontrada, boleto determinado. BDC para FB09 bem-sucedida.
     - **Resultado Esperado:** Log (em `t_log`) contém "FB09: Inclusão de boleto com sucesso no Doc. Contábil [num_doc]".
   - **Cenário 4.3 (Processamento FB09 - Falha Validação BSEG):** Doc. contábil informado não existe na BSEG.
     - **Resultado Esperado:** Log contém "FB09: Doc. Contábil [num_doc]/[empresa]/[ano] não encontrado na BSEG.".
   - **Cenário 4.4 (Chamada Pós-MIRO - Sucesso):** MIRO gerada. Doc. FI encontrado. Boleto da NF original encontrado. BDC para FB09 no doc FI bem-sucedida.
     - **Resultado Esperado:** Log da MIRO (no `FORM zf_executa_miro`) deve conter "Boleto vinculado com sucesso." ao lado da mensagem do doc. MIRO gerado.
   - **Cenário 4.5 (Chamada Pós-MIRO - Boleto NF Original não encontrado):** MIRO gerada, doc FI encontrado, mas boleto da NF original não está na `ZTBMM_NF_FORN_CB`.
     - **Resultado Esperado:** Nenhuma chamada FB09 para o doc FI. Log da MIRO não menciona vínculo de boleto para este caso (a EF diz "Utilizar o valor carregado em tela no SHDB", o que não se aplica aqui; a lógica implementada não tenta adivinhar um boleto).

### 5. Ponto 5: Alteração de Posição de Mensagem de Erro
   - **Ajuste Implementado:** A especificação requer mover um bloco de validação para antes de um `IF l_error`. Sem o código exato e a marcação visual, este ajuste é conceitual e deve ser feito pelo desenvolvedor ABAP, garantindo que a lógica de validação pretendida execute antes da checagem do flag geral de erro.

### 6. Ponto 6 e 7: Ajustes para Lançamento AVD e Validação de Fornecedor AVD (`LFB1-ZZAVD`)
   - **Ajuste Implementado:**
     - Declaração de `w_fornecedor_empresa` e variáveis de controle para popup AVD.
     - Leitura de `LFB1-ZZAVD` para `w_fornecedor_empresa-zzavd` (a ser inserida no loop principal de processamento de fornecedores/pedidos, antes das validações AVD).
     - Substituição de `IF s_st_rec-low = 'N'` por `IF w_fornecedor_empresa-zzavd = abap_true` nos pontos indicados (linhas ~1297, ~1562, ~1639, ~2537).
     - Condicionamento do trecho `CONCATENATE V_NUMDOC...` com `IF w_fornecedor_empresa-zzavd = abap_true.` (antes era `s_st_rec-low = 'N'`).
     - Lógica para exibir popup AVD uma única vez (usando `lv_processar_avd`, `lv_cancelar_avd`) fora do loop principal, condicionado por `w_fornecedor_empresa-zzavd = abap_true` para pelo menos um item.
     - Alteração de `EXIT` para `CONTINUE` na linha ~2241, e remoção/adaptação do `IF s_st_rec-low = 'N'` na linha ~2228.
   - **Cenário 7.1 (Validação AVD):** Fornecedor do pedido é AVD (`w_fornecedor_empresa-zzavd = abap_true`).
     - **Resultado Esperado:** As lógicas dentro dos `IF`s que agora verificam `w_fornecedor_empresa-zzavd` são executadas. O trecho `CONCATENATE V_NUMDOC...` é executado. O popup AVD (se houver itens AVD) é exibido uma vez. Se o usuário confirmar o popup, o processamento AVD continua; se cancelar, itens AVD são pulados.
   - **Cenário 7.2 (Validação Não AVD):** Fornecedor não é AVD.
     - **Resultado Esperado:** Lógicas condicionadas por `w_fornecedor_empresa-zzavd = abap_true` são puladas.
   - **Cenário 7.3 (Linha ~2241):** Dentro de um loop, se a condição `w_fornecedor_empresa-zzavd = abap_true` for atendida e a lógica interna levar ao ponto onde antes havia `EXIT`.
     - **Resultado Esperado:** O processamento `CONTINUE` para a próxima iteração do loop, em vez de sair completamente.

### 8. Ponto 8: Validação de Condição de Pagamento (Bloqueio por Parâmetro `BLOQ_CONDPG`)
   - **Ajuste Implementado:** Adicionada leitura do parâmetro `BLOQ_CONDPG` da `ZGLBC068` para o range `r_condpag_permitida` no `FORM zf_busca_parametros`. A validação `IF w_headerdata-pmnttrms NOT IN r_condpag_permitida` deve ser inserida no `FORM zf_executa_miro` após `w_headerdata-pmnttrms` ser preenchido.
   - **Cenário 8.1:** `BLOQ_CONDPG` na `ZGLBC068` contém 'Z001', 'Z002'. Pedido com `w_headerdata-pmnttrms = 'Z003'`. 
     - **Resultado Esperado:** Mensagem "Condição de pagamento Z003 não foi habilitada para o automatizado" no log. `CONTINUE` para o próximo item.
   - **Cenário 8.2:** `BLOQ_CONDPG` contém 'Z001', 'Z002'. Pedido com `w_headerdata-pmnttrms = 'Z001'`. 
     - **Resultado Esperado:** Processamento continua.

## Conclusão da Validação Conceitual

Os ajustes implementados no arquivo `ZGLMM436_revisado.txt` e detalhados no arquivo `pontos_de_ajuste.md` parecem cobrir conceitualmente os requisitos da especificação funcional. No entanto, a validação final e completa requer testes unitários e integrados em um ambiente SAP real por um desenvolvedor ABAP. Alguns pontos, como a exata localização para inserir certas lógicas (ex: Ponto 1 no SELECT principal, Ponto 3 na validação de moeda) e a interface de usuário para o Ponto 4 (tela de boletos), ainda necessitam da intervenção e expertise do desenvolvedor ABAP durante a implementação final e testes.

O código revisado inclui comentários indicando os pontos de ajuste e as lógicas adicionadas para facilitar a revisão e finalização pelo desenvolvedor.

