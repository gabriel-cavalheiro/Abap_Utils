CLASS zclfi0132_tf_extrat_bancario DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_data_bank FOR TABLE FUNCTION zcdsfi_tf_extrat_bancario.

    INTERFACES if_amdp_marker_hdb .

    TYPES:
      BEGIN OF y_febko,
        bukrs TYPE  febko-bukrs,
        hkont TYPE  febko-hkont,
        hbkid TYPE  febko-hbkid,
        hktid TYPE  febko-hktid,
        aznum TYPE  febko-aznum,
        azdat TYPE  febko-azdat,
        waers TYPE  febko-waers,
        kukey TYPE  febko-kukey,
        ssbtr TYPE  febko-ssbtr,
        ssvoz TYPE  febko-ssvoz,
        esbtr TYPE  febko-esbtr,
        esvoz TYPE  febko-esvoz,
      END OF y_febko,

      BEGIN OF y_febep,
        esnum TYPE  febep-esnum,
        valut TYPE  febep-valut,
        bvdat TYPE  febep-bvdat,
        sgtxt TYPE  febep-sgtxt,
        butxt TYPE  febep-butxt,
        kwbtr TYPE  febep-kwbtr,
        epvoz TYPE  febep-epvoz,
        vgext TYPE  febep-vgext,
      END OF y_febep.

    TYPES:
      t_febko TYPE TABLE OF y_febko,
      t_febep TYPE TABLE OF y_febep.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclfi0132_tf_extrat_bancario IMPLEMENTATION.
  METHOD get_data_bank BY DATABASE
                       FUNCTION FOR HDB
                       LANGUAGE SQLSCRIPT
                       OPTIONS READ-ONLY
                       USING febko febep.

    t_febko = select DISTINCT mandt,
    aznum,
    bukrs,
    hkont,
    hbkid,
    hktid,
    azdat,
    waers,
    kukey,
    ssbtr,
    ssvoz,
    esbtr,
    esvoz
from febko
where mandt = :iv_clnt
AND bukrs  = :iv_bukrs
ORDER BY hkont, azdat asc;

t_febko = apply_filter(:t_febko, :iv_hbkid);
t_febko = apply_filter(:t_febko, :iv_hktid);
t_febko = apply_filter(:t_febko, :iv_where_azdat);
t_febko = apply_filter(:t_febko, :iv_where_hkont);

RETURN select DISTINCT f.mandt,
    t.bukrs,
    t.hkont,
    t.hbkid,
    t.hktid,
    t.aznum,
    t.azdat,
    t.waers,
    t.kukey,
    t.ssvoz,
-- Saldo Inicial do Extrato (ssbtr) e Saldo Final do Extrato (esbtr) com sinal ajustado
  case when t.ssvoz = 'S' THEN t.ssbtr * -1
    else t.ssbtr
  end as ssbtr,

  case when t.esvoz = 'S' THEN t.esbtr * -1
    else t.esbtr
  end as esbtr,

-- Valor do Movimento (kwbtr) com sinal ajustado
    case when f.epvoz = 'S' THEN f.kwbtr * -1
    else f.kwbtr
  end as kwbtr,
    t.esvoz,
    f.esnum,
    f.valut,
    f.bvdat,
    f.sgtxt,
    f.butxt,
    f.epvoz,
    f.vgext
 from febep as f
 inner join :t_febko as t
 on f.kukey = t.kukey;

  endmethod.
ENDCLASS.
