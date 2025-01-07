# 📂 Upload de Arquivos RAP  

O modelo **RAP** (*Restful Application Programming Model*) trouxe funcionalidades nativas para upload de arquivos, como PDFs e planilhas Excel, diretamente em aplicações SAP, facilitando a manipulação de **grandes objetos** (*LOBs*).  

Este guia destaca os passos essenciais e conceitos-chave para integrar essa funcionalidade em seus projetos RAP.  

---

## 🛠️ Passo 1: Criar uma Tabela para LOBs  

Primeiro, é necessário criar uma tabela para armazenar os objetos grandes (**LOBs**). Essa tabela deve conter colunas para o documento, tipo MIME e nome do arquivo:  

~~~abap  
document              : RAWSTRING;  
mimetype              : CHAR128;  
filename              : CHAR128;  
~~~  

---

## 🏗️ Habilitar Upload de Arquivos  

O próximo passo é criar a **CDS Root** que selecionará os dados da tabela, adicionando as **anotações** responsáveis pelo upload de arquivos.  

~~~abap  
@Semantics.largeObject:  
{ mimeType: 'MimeType',  
  fileName: 'Filename',  
  contentDispositionPreference: #INLINE }  
document              as Document,  

@Semantics.mimeType: true  
mimetype              as MimeType,  
filename              as Filename,  
~~~  

---

## 🎯 Posso Limitar os Tipos de Arquivos?  

Sim, é possível restringir os tipos de arquivos que podem ser enviados, utilizando a propriedade `acceptableMimeTypes`.  

~~~abap  
@Semantics.largeObject: { mimeType: 'MimeType',  
                          fileName: 'Filename',  
                          acceptableMimeTypes: [ 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' ],  
contentDispositionPreference: #INLINE }  
Document,  
~~~  

### ⚙️ Configurações:  
- **Qualquer tipo de arquivo:** Deixe o campo vazio ou utilize `*`.  
- **Tipos específicos (ex.: imagens):** Use `image/*`.  

---

## 🔍 Quais MIME Types Posso Preencher?  

Para descobrir os MIME Types corretos, utilize a função **`SDOK_MIMETYPE_GET`**, passando a extensão do arquivo desejado. Ela retornará o tipo MIME que você precisa preencher.  

![image](https://github.com/user-attachments/assets/a54b213a-b1f6-4bb9-b6a1-84fbd35f59b6)

---

## ⚡ Behavior  

Embora não seja necessária uma tratativa específica para o arquivo, o **Behavior** permite adicionar funcionalidades extras, como:  

1. **Validação de dados ao salvar**  
2. **Processamento do arquivo**  

~~~abap  
// Exemplo de validação no Behavior  
validate save:  
  if document IS INITIAL.  
    failed entity action message 'Documento inválido!'.  
  endif.  
~~~  

Com essas etapas, você estará pronto para implementar upload de arquivos com RAP de maneira eficiente e segura. 🚀  
