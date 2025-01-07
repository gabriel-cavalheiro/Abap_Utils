# 📘 Eventos em ABAP para REPORTs

Os eventos ABAP são como capítulos em uma história: cada um tem seu momento certo para brilhar e dar vida ao seu programa.

Se você já escreveu um `REPORT` no ABAP, com certeza esbarrou em eventos como `INITIALIZATION`, `START-OF-SELECTION` e `END-OF-SELECTION`.

Neste artigo, vamos explorar o que são, por que usamos, quando utilizá-los e como colocá-los em prática.

---

## 🎯 Por que Utilizamos Eventos?

Os eventos em ABAP são os pontos de controle que definem o fluxo de execução do programa. Eles permitem organizar o código de forma clara e lógica, garantindo que as ações sejam executadas na ordem correta.

É como planejar uma viagem: você precisa preparar tudo antes (`INITIALIZATION`), começar a jornada (`START-OF-SELECTION`) e, no final, organizar os resultados (`END-OF-SELECTION`).

---

## 📝 Guia Completo: Eventos em ABAP para REPORTs

Os eventos em ABAP são como marcadores de capítulos em um livro. Eles controlam o que acontece e quando acontece em um `REPORT`, permitindo que o programa seja executado de maneira organizada e previsível. Aqui vamos explorar todos os eventos que você pode encontrar em um `REPORT`, abordando:

- Por que utilizamos
- Quando utilizar
- Como utilizar

---

### 🚀 Eventos do Ciclo de Vida de um REPORT

#### 1. `LOAD-OF-PROGRAM`
- **Por que utilizamos?**  
  Executado automaticamente ao carregar o programa na memória. Ideal para inicializações globais.
- **Quando utilizar?**  
  Definir variáveis globais, carregar tabelas em memória ou configurar elementos antes mesmo da exibição da tela de seleção.
- **Como utilizar?**
~~~  
  LOAD-OF-PROGRAM.
    WRITE: / 'O programa foi carregado!'.
~~~

#### 2. `INITIALIZATION`
- **Por que utilizamos?**  
Para inicializar valores padrão para parâmetros ou seleções antes da tela de seleção ser exibida.
- **Quando utilizar?**
Use quando precisar preencher campos da tela de seleção automaticamente ou definir comportamentos padrão.
- **Como utilizar?**
~~~
INITIALIZATION.
  p_data = sy-datum. "Preenche data padrão
  p_user = sy-uname. "Preenche com o usuário atual
~~~

#### 3. `AT SELECTION-SCREEN`
- **Por que utilizamos?**  
Para validar os dados inseridos na tela de seleção.
- **Quando utilizar?**
Sempre que precisar verificar se os dados do usuário são válidos ou ajustar campos com base nas entradas.
- **Como utilizar?**

~~~
AT SELECTION-SCREEN.
  IF p_data < sy-datum.
    MESSAGE 'A data não pode ser no passado' TYPE 'E'.
  ENDIF.
~~~

#### 4. `START-OF-SELECTION`
- **Por que utilizamos?**  
Esse é o coração do programa, onde a lógica principal acontece.
- **Quando utilizar?**
Quando precisar processar dados após a validação da tela de seleção.
- **Como utilizar?**

~~~
START-OF-SELECTION.
  SELECT * FROM mara INTO TABLE lt_mara WHERE matnr = p_matnr.
~~~

#### 5. `GET (para Logical Databases)`
- **Por que utilizamos?**  
Para manipular dados provenientes de uma Logical Database.
- **Quando utilizar?**
Apenas em REPORTs que utilizam Logical Databases.
- **Como utilizar?**

~~~
GET mara.
  WRITE: / mara-matnr, mara-maktx.
~~~

#### 6. `END-OF-SELECTION`
- **Por que utilizamos?**  
Para realizar tarefas de finalização, como formatação de saída ou cálculos finais.
- **Quando utilizar?**
Use após processar todos os dados para consolidar ou exibir os resultados.
- **Como utilizar?**

~~~
END-OF-SELECTION.
  LOOP AT lt_mara INTO ls_mara.
    WRITE: / ls_mara-matnr, ls_mara-maktx.
  ENDLOOP.
~~~

Eventos Relacionados à Tela de Seleção
#### 7. `AT SELECTION-SCREEN OUTPUT`
- **Por que utilizamos?**  
Para ajustar a tela de seleção antes de ela ser exibida.
- **Quando utilizar?**
Use quando precisar ocultar, exibir ou ajustar campos dinamicamente.
- **Como utilizar?**

~~~
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_USER'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
~~~

#### 8. `AT SELECTION-SCREEN ON <CAMPO>`
- **Por que utilizamos?**  
Para validar um campo específico da tela de seleção.
- **Quando utilizar?**
Sempre que precisar de regras específicas para um único campo.
- **Como utilizar?**

~~~
AT SELECTION-SCREEN ON p_data.
  IF p_data < sy-datum.
    MESSAGE 'Data inválida!' TYPE 'E'.
  ENDIF.
~~~

#### 9. `AT SELECTION-SCREEN ON BLOCK`
- **Por que utilizamos?**  
Para manipular ou validar um bloco inteiro na tela de seleção.
- **Quando utilizar?**
Quando houver uma relação entre campos em um bloco.
- **Como utilizar?**

~~~
AT SELECTION-SCREEN ON BLOCK b1.
  IF p_data IS INITIAL AND p_user IS INITIAL.
    MESSAGE 'Preencha pelo menos um campo do bloco!' TYPE 'E'.
  ENDIF.
~~~

Eventos Menos Usuais, Mas Úteis
#### 10. `TOP-OF-PAGE`
- **Por que utilizamos?**  
Para criar um cabeçalho customizado no início de cada página de uma lista.
- **Quando utilizar?**
Em programas que geram relatórios ALV ou listas.
- **Como utilizar?**

~~~
TOP-OF-PAGE.
  WRITE: / 'Relatório de Materiais'.
~~~

#### 11. `END-OF-PAGE`
- **Por que utilizamos?**  
Para criar um rodapé customizado no final de cada página de uma lista.
- **Quando utilizar?**
Quando precisar de rodapés em relatórios extensos.
- **Como utilizar?**

~~~
END-OF-PAGE.
  WRITE: / 'Fim da Página', sy-pagno.
~~~

Eventos Específicos para INTERACTIVE REPORTING
#### 12. `AT LINE-SELECTION`
- **Por que utilizamos?**  
Para capturar a ação de clique em uma linha de relatório interativo.
- **Quando utilizar?**
Em relatórios que têm navegação detalhada.
- **Como utilizar?**

~~~
AT LINE-SELECTION.
  READ TABLE lt_mara INDEX sy-lilli INTO ls_mara.
  WRITE: / 'Material Selecionado:', ls_mara-matnr.
~~~

#### 13. `AT USER-COMMAND`
- **Por que utilizamos?**  
Para capturar comandos do usuário, como botões personalizados.
- **Quando utilizar?**
Sempre que precisar de interações personalizadas.
- **Como utilizar?**

~~~
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'SAIR'.
      LEAVE PROGRAM.
    WHEN 'DETALHES'.
      WRITE: / 'Detalhes do Material'.
  ENDCASE.
~~~

#### 14. `TOP-OF-PAGE DURING LINE-SELECTION`
- **Por que utilizamos?**  
Para criar cabeçalhos em relatórios interativos detalhados.
- **Quando utilizar?**
Sempre que uma página detalhada precisar de cabeçalhos.
- **Como utilizar?**

~~~
TOP-OF-PAGE DURING LINE-SELECTION.
  WRITE: / 'Detalhes da Seleção'.   
~~~
