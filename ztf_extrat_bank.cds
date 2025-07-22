@EndUserText.label: 'Table Function para Extrato Bancário'
@AccessControl.authorizationCheck: #NOT_REQUIRED

define table function ZCDSFI_TF_EXTRAT_BANCARIO
  with parameters
    @Environment.systemField: #CLIENT
    iv_clnt        : abap.clnt,
    iv_bukrs       : bukrs,
    iv_hbkid       : string_data,
    iv_hktid       : string_data,
    iv_where_hkont : string_data, // Range de Razão
    iv_where_azdat : string_data  // Range de Data Extrato
returns
{
  mandt : mandt;
  bukrs : bukrs;
  hkont : hkont;
  hbkid : hbkid;
  hktid : hktid;
  aznum : aznum_eb;
  azdat : azdat_eb;
  @Semantics.currencyCode: true
  waers : waers;
  @Semantics.amount.currencyCode: 'waers'
  kukey : kukey_eb;
  ssvoz : ssvoz_eb;
  @Semantics.amount.currencyCode: 'waers'
  ssbtr : ssbtr_eb;
  @Semantics.amount.currencyCode: 'waers'
  esbtr : esbtr_eb;
  @Semantics.amount.currencyCode: 'waers'
  kwbtr : kwbtr_eb;
  esvoz : esvoz_eb;
  esnum : esnum_eb;
  valut : valut_eb;
  bvdat : bvdat_eb;
  sgtxt : sgtxt;
  butxt : butxt_eb;
  epvoz : epvoz_eb;
  vgext : vgext_eb;
}
implemented by method
  ZCLFI0132_TF_EXTRAT_BANCARIO=>get_data_bank;
