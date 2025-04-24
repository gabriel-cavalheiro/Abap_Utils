# Pseudo-campos RAP/EML

Descrição detalhada dos pseudo-campos gerados pelo framework RAP/EML, para uso em operações CRUD e Actions.

---

## 🔑 Chaves e Identificadores

-  **`%cid`** – *Client ID*  
  Cada registro enviado pelo cliente recebe um CID único. Serve para correlacionar, após a operação, qual entrada gerou qual resposta (mapped / failed / reported).

-  **`%cid_ref`** – *Client ID de referência*  
  Em operações “CREATE BY _assoc”, vincula o registro filho ao pai, garantindo a correlação hierárquica no retorno.

-  **`%pid`** – *Preliminary ID*  
  Em cenários de *late numbering*, fornece um ID provisório antes da geração da chave definitiva no banco.

-  **`%tky`** – *Transient Key*  
  Chave transitória atribuída a cada instância durante a transação; após o commit, converte-se na chave primária real.

-  **`%key`** – *Chaves primárias consolidadas*  
  Estrutura que agrupa todas as chaves primárias da entidade (por ex.: `ID`, `COMPANY_CODE`, `YEAR`).

-  **`%tmp`** – *Variável temporária interna*  
  Área reservada para dados auxiliares do processamento, sem persistência no banco.

---

## 🔄 Processamento e Fluxo de Dados

-  **`%data`** – *Payload real de dados*  
  Agrupa todos os campos definidos na CDS, sem campos de controle.

-  **`%op`** – *Operação a executar*  
  Indica se cada linha em `MODIFY ENTITIES` será **CREATE**, **UPDATE** ou **DELETE**.

-  **`%element`** – *Detalhe de campo em updates dinâmicos*  
  Lista quais campos foram modificados em `MODIFY … SET FIELDS`, otimizando o SQL gerado.

-  **`%fail`** – *Dados que falharam*  
  Contém registros que não foram processados com sucesso, junto com sua `%cid` ou `%tky`.

-  **`%msg`** – *Mensagens do framework*  
  Agrupa erros, avisos e informações gerados pelo RAP (validações, transições, etc.).

-  **`%state_area`** – *Transições de estado*  
  Registra as mudanças de status definidas no Behavior Definition (ex.: “submetido → aprovado”).

-  **`%other`** – *Uso interno*  
  Componente genérico para dados de bastidores do RAP, sem aplicação direta em lógica de negócio.

-  **`%global`** – *Features globais*  
  Lista as features habilitadas (`features : global`) para a operação em questão.

-  **`%path`** – *Caminho de navegação*  
  Indica a rota de acesso em composições e associações (ex.: `RootEntity/ToItems/ToDetails`).

---

## 🚀 Associações, Ações e Controles

-  **`%action`** – *Dados de ACTION*  
  Parâmetros de entrada e saída de uma ação (`…EXECUTE`).

-  **`%assoc`** – *Dados de associação*  
  Usado em `CREATE BY _assoc` para enviar entidades filhas junto com o pai.

-  **`%control`** – *Sinalizadores de controle*  
  Flags que indicam, em UPDATE, quais campos devem ou não ser considerados.

-  **`%create`** – *Dados para CREATE FROM*  
  Payload original que será criado antes do mapeamento interno do RAP.

-  **`%delete`** – *Dados para DELETE*  
  Chaves ou IDs dos registros a serem removidos.

-  **`%features`** – *Features específicas*  
  Habilidades avançadas habilitadas (drafts, late numbering, deep insert etc.).

-  **`%perm`** – *Permissões*  
  Resultado de `GET PERMISSIONS`, indicando direitos de leitura, escrita e exclusão.

-  **`%target`** – *Alvo de associação*  
  Identifica o registro pai ou referência em operações de associação.

-  **`%param`** – *Parâmetros de importação/exportação*  
  Valores adicionais em ações ou leituras especiais (`READ … EXECUTE`).

-  **`%own`** – *Propriedade do registro*  
  Controle interno de quem é o dono final do objeto durante a execução.

-  **`%update`** – *Campos efetivamente alterados*  
  Enumera os campos que o RAP detectou como modificados, minimizando o tráfego de dados.

---
