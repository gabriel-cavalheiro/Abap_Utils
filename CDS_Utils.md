# Comandos e Snippets ADT

## Sites Utéis

<https://sapcodes.com/2024/07/08/rap-1-eml-read-and-action/>

<https://medium.com/@vitorkazuma/criando-crud-rap-com-gerador-adt-integrado-em-poucos-cliques-2396925d69eb>

## Remover dados duplicados da seleção da CDS com select distinct 🧮

```cds

define view entity Z_CDS_EX
 as select distinct from ztbexemplo
{
 key abap.char 'A' as Field
}

```

## Função para conversão de moedas e unidade de medidas 💵🪙📐

```cds

define view entity Z_CDS_EX
 with parameters //parametro de entrada
  P_DisplayUnit : msehi
 as select distinct from zi_exemplo
{
 key SalesOrder,
 key SalesOrderItem,
 @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
 OrderQuantity,
 OrderQuantityUnit,
 @Semantics.quantity.unitOfMeasure: 'OrderQuantityDisplayUnit' //unidade alvo
 unit_conversion( quantity       => OrderQuantity,
                  source_unit    => OrderQuantityUnit,
                  target_unit    => $parameters.P_DisplayUnit,
                  error_handling => 'FAIL_ON_ERROR' )
 as OrderQuantityInDisplayUnit,
$parameters.P_DisplayUnit as OrderQuantityDisplayUnit
}

```

```cds

define view entity Z_CDS_EX
 with parameters 
  P_DisplayCurrency : waers_curc,
  P_ExchangeRateDate : sydatum
 as select distinct from zi_exemplo
{
 key SalesOrder,
 key SalesOrderItem,
 @Semantics.quantity.currencyCode: 'TransactionCurrency'
 NetAmount,
 TransactionCurrency,
 @Semantics.quantity.currencyCode: 'TransactionCurrency' //moeda alvo
 currency_conversion(
              amount             => NetAmount,
              source_currency    => TransactionCurrency,
              target_currency    => $parameters.P_DisplayCurrency,
              exchange_rate_date => $parameters.P_ExchangeRateDate,
              exchange_rate_type => $parameters.P_DisplayUnit,
              round              => 'M' 
              decimal_shift      => 'X'
              decimal_shift_back => 'X'
              error_handling     => 'SET_TO_NULL' )
 as NetAmountInDisplayCurrency,
$parameters.P_DisplayCurrency as DisplayCurrency
} 


```

## Ações de Edição

| Função | Atalho |
| --- | --- |
| *Buscar objetos dentro do eclipse (CDS, Tables, etc.)* | Ctrl + Shift + a |
| Duplicar linha | Ctrl + Alt + up/down |
| Ativar objeto de desenvolvimento inativo | Ctrl + F3 |
| Ativar todos os objetos de desenvolvimento inativos | Ctrl + Shift + F3 |
| Verificar consistência e sintaxe | Ctrl + F2 |
| Fechar | Ctrl + W |
| Fechar tudo | Ctrl + Shift + W |
| Conclusão de código / Assistência de conteúdo | Ctrl + Espaço |
| Excluir linha | Ctrl + D |
| Excluir próxima palavra | Ctrl + Delete |
| Excluir palavra anterior | Ctrl + Backspace |
| Encontrar próximo | Ctrl + K |
| Encontrar anterior | Ctrl + Shift + K |
| Formatar código-fonte (também conhecido como Pretty Printer no SAP GUI) | Shift + F1 |
| Formatar bloco de código-fonte (também conhecido como Pretty Printer no SAP GUI) | Ctrl + Shift + F1 |
| Conclusão de palavra-chave | Tab |
| Marcar palavra | Duplo-clique |
| Marcar linha inteira | Triplo-clique |
| Novo objeto de desenvolvimento ABAP | Ctrl + N |
| Abrir correção rápida / diálogo de assistência rápida | Ctrl + 1 |
| Renomear | Alt + Shift + R |
| Salvar | Ctrl + S |
| Salvar tudo | Ctrl + Shift + S |
| Selecionar para maiúsculas | Ctrl + Shift + X |
| Selecionar para minúsculas | Ctrl + Shift + Y |

# Ações de Exibição

| Função | Atalho |
| --- | --- |
| Mostrar as propriedades do objeto ou arquivo focado atualmente | Alt + Ctrl + P |
| Mostrar marcadores, pontos de interrupção e tarefas da linha do editor focado | Alt + Ctrl + P |
| Mostrar a visualização de variáveis no depurador e definir o foco nela. <br> *Nota:* Volte para o código-fonte usando F12. | Shift + Alt + V |
| Definir o menu de visualização, onde o menu de contexto da régua dos editores de texto é mostrado ou para adicionar marcadores, pontos de interrupção ou tarefas | Ctrl + F10 |
| Mostrar marcadores, pontos de interrupção e tarefas da linha do editor focado | Alt + Ctrl + F10 |
| Ajustar o tamanho do editor atual, visualização, etc., ao tamanho máximo da tela | Ctrl + M |

# Ações de Navegação

| Função | Atalho |
| --- | --- |
| Navegação para trás para a aba aberta anteriormente | Alt + Esquerda |
| Navegação para frente para a próxima aba aberta | Alt + Direita |
| Selecionar o próximo editor aberto para navegar | Ctrl + F6 |
| Alternar entre perspectivas | Ctrl + F8 |
| Selecionar o editor aberto anterior para navegar | Ctrl + Shift + F6 |
| Navegar para a última localização editada | Ctrl + Q |
| Abrir objeto de desenvolvimento | Ctrl + Shift + A |
| Abrir Resumo Rápido | Ctrl + O |
| Abrir Hierarquia de Tipo Rápida | Ctrl + T |
| Abrir SAP GUI | Ctrl + 6 |
| Alternar para a próxima visualização | Ctrl + F7 |
| Alternar para a próxima perspectiva | Ctrl + F8 |
| Navegar para o código-fonte ABAP | F3 ou Ctrl + Clique |
| Mover o foco do teclado para o editor principal ou a aba do editor ativo | F12 |
| Mostrar menu de contexto | Shift + F10 |

# Ações de Movimento

| Função | Atalho |
| --- | --- |
| Retroceder uma palavra | Ctrl + Esquerda |
| Avançar uma palavra | Ctrl + Direita |
| Pular para linha | Ctrl + L |
| Mover uma linha para baixo | Alt + Baixo |
| Mover uma linha para cima | Alt + Cima |

# Ações de Comentário

| Função | Atalho |
| --- | --- |
| Adicionar comentários | Ctrl + < |
| Remover comentários | Ctrl + > |
| Alternar comentários | Ctrl + 7 |

# Busca e Ajuda

| Função | Atalho |
| --- | --- |
| Abrir diálogo de busca | Ctrl + H |
| Mostrar ajuda da linguagem ABAP | F1 |
| Mostrar informações de elemento ABAP | F2 |
| Lista de onde foi usado | Ctrl + Shift + G |

# Atalhos Baseados em Windows

| Função | Atalho |
| --- | --- |
| Localizar / Substituir texto | Ctrl + F |
| Copiar seleção | Ctrl + C |
| Fechar | Ctrl + F4 |
| Fechar tudo | Ctrl + Shift + F4 |
| Cortar seleção | Ctrl + X |
| Colar seleção | Ctrl + V |
| Refazer digitação | Ctrl + Y |
| Selecionar tudo | Ctrl + A |
| Desfazer digitação | Ctrl + Z |
