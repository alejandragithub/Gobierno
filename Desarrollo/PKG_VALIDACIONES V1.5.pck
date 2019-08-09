CREATE OR REPLACE PACKAGE PKG_VALIDACIONES IS

  PROCEDURE SP_VALIDAR(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE);
  -- Proceso principal de validaciones del programa BEG
  PROCEDURE SP_VALIDAR_BEG;
  -- Proceso principal de validaciones del programa BOS
  PROCEDURE SP_VALIDAR_BOS;
 

  -- reversa de validacion
  PROCEDURE SP_LIMPIAR_VALIDACION(I_FECHA_DESDE               IN DATE,
                                  I_FECHA_HASTA               IN DATE,
                                  I_USUARIO                   IN T_OPERACIONES.USUARIO_OPERACION%TYPE,
                                  I_OBSERVACIONES             IN T_OPERACIONES.OBSERVACIONES%TYPE,
                                  I_LISTA_ID_EMPRESA          IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_ORIGEN           IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_DESTINO          IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_ADMINISTRADORA   IN VARCHAR2 DEFAULT NULL,
                                  I_CUIL                      IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_TIPO_EMPRESA     IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_TIPO_SOLICITANTE IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_RESULTADO        IN VARCHAR2 DEFAULT NULL,
                                  I_LISTA_ID_VALIDACIONES     IN VARCHAR2 DEFAULT NULL,
                                  I_PROGRAMA                  IN T_MOVIMIENTOS.ID_PROGRAMA%TYPE,
                                  I_VERSION                   IN T_MOVIMIENTOS.NRO_VERSION%TYPE DEFAULT NULL);

  -- Autorizacion no existente en la base de gobierno
  FUNCTION FU_VALIDAR_AUTORIZACION_INEX(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;
  -- Autorizacion inconsistente
  FUNCTION FU_VALIDAR_AUTORIZACION_INCONS(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- CUIL autorizado
  FUNCTION FU_VALIDAR_CUIL_AUTORIZADO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- FUERA DE VIGENCIA DE AUTORIZACION
  FUNCTION FU_VALIDAR_AUTORIZACION_VIGEN(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- TIPO DE BOLETO URBANO
  FUNCTION FU_VALIDAR_TIPO_LINEA_ATOS(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Distinta empresa mismo grupo ambas urbanas
  FUNCTION FU_VALIDAR_DIS_EMP_GRUPO_URB(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Distinta empresa distinto grupo
  FUNCTION FU_VALIDAR_DIS_EMP_DIS_GRUPO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- misma empresa distinto grupo al informado
  FUNCTION FU_VALIDAR_MIS_EMP_DIS_GRP_INF(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar tramo no autorizado
  FUNCTION FU_VALIDAR_TRAMO_NO_AUT(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar empresa inexistente en base de gobierno
  FUNCTION FU_VALIDAR_EMPRESA_INEXISTENTE(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar tipo de solicitante nulo
  FUNCTION FU_VALIDAR_TIPO_SOL_NULO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar boleto urbano contra empresa no urbana
  FUNCTION FU_VALIDAR_URB_EMP_NOURB(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar urbano contra autorizacion no urbana
  FUNCTION FU_VALIDAR_URB_AUT_NOURB(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar horario autorizado
  FUNCTION FU_VALIDAR_FRANJA_HORARIA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- VALIDAR FERIADO LOCAL URBANO
  FUNCTION FU_VALIDAR_FERIADO_LOCAL(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Validar Feriado General
  FUNCTION FU_VALIDAR_FERIADO_GENERAL(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar feriado menor a 100km
  FUNCTION FU_VALIDAR_FERIADO_MENOR_100(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar feriado mayor a 100km
  FUNCTION FU_VALIDAR_FERIADO_MAYOR_100(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar si el origen y/o destino informado son nulos
  FUNCTION FU_VALIDAR_ORI_DES_NULO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Verificaciones Duplicadas
  FUNCTION FU_VALIDAR_VERIFICACION_DUP(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Boletos Duplicados
  FUNCTION FU_VALIDAR_BOLETO_DUP(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Empresa nula o negativa
  FUNCTION FU_VALIDAR_EMPRESA_NULA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Autorizacion nula o negativa
  FUNCTION FU_VALIDAR_AUTORIZACION_NULA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- CUIL nulo o malformado
  FUNCTION FU_VALIDAR_CUIL_MALFORMADO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Boleto Interurbano con Autorizacion Urbana
  FUNCTION FU_VALIDAR_INTER_AUT_URBANO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Boleto interurbano con empresa urbana
  FUNCTION FU_VALIDAR_INTER_EM_URBANO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar tarjeta en lista negra
  FUNCTION FU_VALIDAR_LISTA_NEGRA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- boleto mismo grupo empresa interurbana o diferencial con autorizacion urbana
  FUNCTION FU_VALIDAR_SERV_INTER(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- boleto mismo grupo empresa urbana autorizacion interurbana o diferencial
  FUNCTION FU_VALIDAR_SERV_URBANO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- Valida que la tarjeta informada coincida con la solicitud para la fecha de viaje
  FUNCTION FU_VALIDAR_TARJETA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar usos autorizados
  FUNCTION FU_VALIDAR_USOS_AUTORIZADOS(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar vencimiento del tramo autorizado
  FUNCTION FU_VALIDAR_FUERA_TRIMESTRE(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar empresa fuera de servicio
  FUNCTION FU_VALIDAR_EMP_FUERA_SERVICIO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- validar boleto durante receso escolar
  FUNCTION FU_VALIDAR_RECESO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER;

  -- aprobar boletos por lote
  PROCEDURE SP_APROBAR_BOLETOS(I_USUARIO                   IN T_OPERACIONES.USUARIO_OPERACION%TYPE,
                               I_OBSERVACIONES             IN T_OPERACIONES.OBSERVACIONES%TYPE DEFAULT NULL,
                               I_FEC_MOVIMIENTO_DESDE      IN T_MOVIMIENTOS.FEC_MOVIMIENTO%TYPE,
                               I_FEC_MOVIMIENTO_HASTA      IN T_MOVIMIENTOS.FEC_MOVIMIENTO%TYPE,
                               I_CUIL                      IN T_MOVIMIENTOS.CUIL%TYPE DEFAULT NULL,
                               I_LISTA_ID_ORIGEN           IN VARCHAR2 DEFAULT NULL,
                               I_LISTA_ID_DESTINO          IN VARCHAR2 DEFAULT NULL,
                               I_LISTA_ID_EMPRESA          IN VARCHAR2 DEFAULT NULL,
                               I_LISTA_ID_ADMINISTRADORA   IN VARCHAR2 DEFAULT NULL,
                               I_LISTA_ID_TIPO_EMPRESA     IN VARCHAR2 DEFAULT NULL,
                               I_LISTA_ID_TIPO_SOLICITANTE IN VARCHAR2 DEFAULT NULL,
                               I_LISTA_ID_VALIDACIONES     IN VARCHAR2 DEFAULT NULL);


END PKG_VALIDACIONES;
/
CREATE OR REPLACE PACKAGE BODY PKG_VALIDACIONES IS

  /*************************************************************************
  * VALIDADOR POR BOLETO                                                   *
  * ====================                                                   *
  *                                                                        *
  * Corre para el boleto indicado todas las validaciones vigentes segun su *
  * programa, en orden de prioridad. En caso de que una validacion que     *
  * genere rechazo no pase el proceso corta en esa validacion ignorando    *
  * las siguientes, para los casos de validaciones que generen observacion *
  * se corren todas sin considerar el resultado.                           *
  *                                                                        *
  *************************************************************************/
  PROCEDURE SP_VALIDAR(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE) IS

    V_SQL              VARCHAR2(4000);
    V_RESULTADO        INTEGER;
    V_VALIDACIONACTUAL T_VALIDACIONES.N_VALIDACION%TYPE;
    FLAG_RECHAZO       VARCHAR2(1) := 'N';
    FLAG_FILTRO        VARCHAR2(1) := 'N';

    PRAGMA AUTONOMOUS_TRANSACTION;

  BEGIN

    IF I_ID_MOVIMIENTO IS NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_ID_MOVIMIENTO no puede ser NULO');
    END IF;

    -- Buscar datos de validacion a ejecutar
    FOR VAL IN (SELECT VAL.ID_VALIDACION
                      ,VAL.N_VALIDACION
                      ,VAL.PROCEDIMIENTO
                      ,VPR.RECHAZA_SN
                      ,VAL.COD_RECHAZO
                      ,VPR.PRIORIDAD
                  FROM T_VALIDACIONES           VAL
                      ,T_VALIDACIONES_PROGRAMA  VPR
                      ,T_MOVIMIENTOS            MOV
                      ,T_PERIODOS_LIQUIDACION   PER
                 WHERE MOV.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                   AND VPR.ID_PROGRAMA = MOV.ID_PROGRAMA
                   AND VPR.ID_VALIDACION = VAL.ID_VALIDACION
                   AND MOV.FEC_MOVIMIENTO BETWEEN PER.FEC_DESDE AND
                       (PER.FEC_HASTA + 1) - 1 / 24 / 60 / 60
                   AND SYSDATE BETWEEN VPR.VIGENCIA_DESDE AND VPR.VIGENCIA_HASTA
                   AND PER.ID_PROGRAMA = MOV.ID_PROGRAMA
                   AND PER.ID_PERIODO BETWEEN
                       NVL(VPR.PERIODO_DESDE, PER.ID_PERIODO) AND
                       NVL(VPR.PERIODO_HASTA, PER.ID_PERIODO)
                     --  and VAL.ID_VALIDACION=7
                 ORDER BY VPR.PRIORIDAD )
    LOOP

      --  seteo info de sesion
      DBMS_APPLICATION_INFO.SET_ACTION('VALIDACION: ' || VAL.N_VALIDACION ||
                                       ' - BOLETO: ' || I_ID_MOVIMIENTO);

      V_VALIDACIONACTUAL := VAL.N_VALIDACION;

      -- Ejecutar Validacion
      V_SQL := 'BEGIN :b := PKG_VALIDACIONES.' || VAL.PROCEDIMIENTO || '( ' ||
               I_ID_MOVIMIENTO || ' ); END;';

      EXECUTE IMMEDIATE V_SQL
        USING OUT V_RESULTADO;

      -- Si el resultado es 0, no pasa la validacion
      IF V_RESULTADO = 0
      THEN

        INSERT INTO T_MOVIMIENTO_RECHAZO
          (ID_MOVIMIENTO
          ,ID_VALIDACION)
        VALUES
          (I_ID_MOVIMIENTO
          ,VAL.ID_VALIDACION);

        FLAG_RECHAZO := 'S';

        SELECT DECODE(VAL.RECHAZA_SN, 'S', 'S')
          INTO FLAG_FILTRO
          FROM DUAL;

      END IF;

      -- si es una validacion que genera rechazo no sigo validando el resto
      IF VAL.RECHAZA_SN = 'S'
         AND V_RESULTADO = 0
      THEN
        EXIT;
      END IF;

    END LOOP;

    IF FLAG_RECHAZO = 'S'
    THEN

      UPDATE T_MOVIMIENTOS
         SET VALIDADO_SN    = 'S'
            ,ID_RESULTADO   = DECODE(FLAG_FILTRO, 'S', 3, 2)
            ,FEC_VALIDACION = SYSDATE
       WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO;

    ELSE

      UPDATE T_MOVIMIENTOS
         SET VALIDADO_SN    = 'S'
            ,ID_RESULTADO   = 1
            ,FEC_VALIDACION = SYSDATE
       WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO;

    END IF;
    
    COMMIT;

  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE_APPLICATION_ERROR(-20002
                             ,'Error al ejecutar la validacion ' ||
                              V_VALIDACIONACTUAL || ' con ID_MOVIMIENTO = ' ||
                              I_ID_MOVIMIENTO || '. ' || SQLERRM);
  END SP_VALIDAR;

  /***************************************************************************
  * PROCESO PRINCIPAL DE VALIDACION DE BOLETO ESTUDIANTIL GRATUITO           *
  * ==============================================================           *
  *                                                                          *
  * Detecta los boletos informados del prorgama BEG que esten en condiciones *
  * de ser validados, luego para cada boleto corre secuencialmente todas las *
  * validaciones parametrizadas para este programa en orden de prioridad. Se *
  * ignoran los boletos previamente validados y/o liquidados                 *
  *                                                                          *
  ***************************************************************************/
  PROCEDURE SP_VALIDAR_BEG IS

  BEGIN

    FOR BOL_BEG IN (SELECT M.ID_MOVIMIENTO
                      FROM T_MOVIMIENTOS M
                     WHERE M.ID_TIPO_MOVIMIENTO = 1
                       AND M.ID_ESTADO = 1
                       AND M.LIQUIDADO_SN IS NULL
                       AND M.VALIDADO_SN IS NULL
                       AND M.ID_PROGRAMA = 1
                       )
    LOOP

      SP_VALIDAR(I_ID_MOVIMIENTO => BOL_BEG.ID_MOVIMIENTO);

    END LOOP;

  END SP_VALIDAR_BEG;
  
  /***************************************************************************
  * PROCESO PRINCIPAL DE VALIDACION DE BOLETO OBRERO SOCIAL                  *
  * =======================================================                  *
  *                                                                          *
  * Detecta los boletos informados del prorgama BOS que esten en condiciones *
  * de ser validados, luego para cada boleto corre secuencialmente todas las *
  * validaciones parametrizadas para este programa en orden de prioridad. Se *
  * ignoran los boletos previamente validados y/o liquidados                 *
  *                                                                          *
  ***************************************************************************/
  PROCEDURE SP_VALIDAR_BOS IS

  BEGIN

    FOR BOL_BOS IN (SELECT M.ID_MOVIMIENTO
                      FROM T_MOVIMIENTOS M
                     WHERE M.ID_TIPO_MOVIMIENTO = 1
                       AND M.ID_ESTADO = 1
                       AND M.LIQUIDADO_SN IS NULL
                       AND M.VALIDADO_SN IS NULL
                       AND M.ID_PROGRAMA = 2
                       )
    LOOP

      SP_VALIDAR(I_ID_MOVIMIENTO => BOL_BOS.ID_MOVIMIENTO);

    END LOOP;

  END SP_VALIDAR_BOS;

  /*************************************************************************************************
  * LIMPIAR VALIDACION EN LOTE                                                                     *
  *                                                                                                *
  * Vuelve atras el proceso de validacion para el lote indicado, solo considera los boletos que no *
  * han sido liquidados. El proceso autom�tico de validacion va a retomar estos boletos en su      *
  * corrida normal y revalidarlos sin neceisdad de indicarle nada. Omite los boletos que han sido  *
  * aprobados manualmente                                                                          *
  *************************************************************************************************/
  PROCEDURE SP_LIMPIAR_VALIDACION(I_FECHA_DESDE               IN DATE
                                 ,I_FECHA_HASTA               IN DATE
                                 ,I_USUARIO                   IN T_OPERACIONES.USUARIO_OPERACION%TYPE
                                 ,I_OBSERVACIONES             IN T_OPERACIONES.OBSERVACIONES%TYPE
                                 ,I_LISTA_ID_EMPRESA          IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_ORIGEN           IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_DESTINO          IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_ADMINISTRADORA   IN VARCHAR2 DEFAULT NULL
                                 ,I_CUIL                      IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_TIPO_EMPRESA     IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_TIPO_SOLICITANTE IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_RESULTADO        IN VARCHAR2 DEFAULT NULL
                                 ,I_LISTA_ID_VALIDACIONES     IN VARCHAR2 DEFAULT NULL
                                 ,I_PROGRAMA                  IN T_MOVIMIENTOS.ID_PROGRAMA%TYPE
                                 ,I_VERSION                   IN T_MOVIMIENTOS.NRO_VERSION%TYPE DEFAULT NULL) IS

    PRAGMA AUTONOMOUS_TRANSACTION;

    V_SQL          VARCHAR2(10000);
    V_FILTROS      VARCHAR2(10000);
    V_ID_OPERACION T_OPERACIONES.ID_OPERACION%TYPE;

  BEGIN

    -- control parametros
    IF I_FECHA_DESDE IS NULL
       OR I_FECHA_HASTA IS NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'Deben indicarse ambos parametros de vigencia para correr el proceso de reversa de validaciones');

    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_EMPRESA, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_EMPRESA IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_EMPRESA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_ORIGEN, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_ORIGEN IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_ORIGEN tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_DESTINO, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_DESTINO IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_DESTINO tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_ADMINISTRADORA, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_ADMINISTRADORA IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_ADMINISTRADORA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_TIPO_EMPRESA, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_TIPO_EMPRESA IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_TIPO_EMPRESA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_TIPO_SOLICITANTE, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_TIPO_SOLICITANTE IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_TIPO_SOLICITANTE tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_RESULTADO, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_RESULTADO IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_RESULTADO tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_VALIDACIONES, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_VALIDACIONES IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro I_LISTA_ID_VALIDACIONES tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    -- armo los filtros
    IF I_LISTA_ID_EMPRESA IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_EMPRESA IN (' || I_LISTA_ID_EMPRESA || ') ';
    END IF;

    IF I_LISTA_ID_ORIGEN IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_ORIGEN IN (' || I_LISTA_ID_ORIGEN || ') ';
    END IF;

    IF I_LISTA_ID_DESTINO IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_DESTINO IN (' || I_LISTA_ID_DESTINO || ') ';
    END IF;

    IF I_LISTA_ID_ADMINISTRADORA IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_ADMINISTRADORA IN (' ||
                   I_LISTA_ID_ADMINISTRADORA || ') ';
    END IF;

    IF I_CUIL IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.CUIL = ' || I_CUIL;
    END IF;

    IF I_LISTA_ID_TIPO_EMPRESA IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_TIPO_EMPRESA IN(' ||
                   I_LISTA_ID_TIPO_EMPRESA || ') ';
    END IF;

    IF I_LISTA_ID_TIPO_SOLICITANTE IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_TIPO_SOLICITANTE IN(' ||
                   I_LISTA_ID_TIPO_SOLICITANTE || ') ';
    END IF;

    IF I_LISTA_ID_RESULTADO IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_RESULTADO IN(' ||
                   I_LISTA_ID_RESULTADO || ') ';
    END IF;

    IF I_LISTA_ID_VALIDACIONES IS NOT NULL
    THEN
      V_FILTROS := V_FILTROS ||
                   ' AND EXISTS ( SELECT 1
                                     FROM T_MOVIMIENTO_RECHAZO R
                                    WHERE R.ID_MOVIMIENTO = M.ID_MOVIMIENTO
                                      AND R.ID_VALIDACION IN (' ||
                   I_LISTA_ID_VALIDACIONES || ')) ';
    END IF;

    IF I_VERSION IS NOT NULL
    THEN

      V_FILTROS := V_FILTROS || ' AND M.NRO_VERSION = ' || I_VERSION;

    END IF;

    -- genero la operacion
    V_ID_OPERACION := SEQ_OPERACIONES.NEXTVAL;

    INSERT INTO T_OPERACIONES
      (ID_OPERACION
      ,ID_TIPO_OPERACION
      ,USUARIO_OPERACION
      ,FEC_OPERACION
      ,OBSERVACIONES)
    VALUES
      (V_ID_OPERACION
      ,'RV'
      ,I_USUARIO
      ,SYSDATE
      ,I_OBSERVACIONES);

    V_SQL := 'INSERT INTO T_OPERACIONES_MOVIMIENTO
      (ID_OPERACION
      ,ID_MOVIMIENTO)
      SELECT ' || V_ID_OPERACION || '
            ,M.ID_MOVIMIENTO
        FROM T_MOVIMIENTOS M
       WHERE M.FEC_MOVIMIENTO BETWEEN trunc(:FD) AND trunc(:FH+1) -1/24/60/60
         AND M.ID_TIPO_MOVIMIENTO = 1
         AND M.ID_ESTADO = 1
         AND M.LIQUIDADO_SN IS NULL
         AND NVL(M.ID_RESULTADO, 0) != 4 ' || V_FILTROS;

    DBMS_OUTPUT.PUT_LINE(V_SQL);

    EXECUTE IMMEDIATE V_SQL
      USING I_FECHA_DESDE, I_FECHA_HASTA;

    -- reversa en movimientos
    V_SQL := 'UPDATE T_MOVIMIENTOS M
                 SET M.VALIDADO_SN    = NULL
                    ,M.FEC_VALIDACION = NULL
                    ,M.ID_RESULTADO   = NULL
                    ,M.OBSERVACION    = NULL
               WHERE M.FEC_MOVIMIENTO BETWEEN TRUNC(:FD) AND TRUNC(:FH+1) -1/24/60/60
                 AND M.ID_TIPO_MOVIMIENTO = 1
                 AND M.ID_ESTADO = 1
                 AND M.LIQUIDADO_SN IS NULL
                 AND M.ID_PROGRAMA = ' || I_PROGRAMA ||
                 ' AND NVL(M.ID_RESULTADO,0) != 4' || V_FILTROS;

    DBMS_OUTPUT.PUT_LINE(V_SQL);

    EXECUTE IMMEDIATE V_SQL
      USING I_FECHA_DESDE, I_FECHA_HASTA;

    -- si no encuentra boletos descarto los cambions y corto el proceso
    IF SQL%ROWCOUNT = 0
    THEN
      ROLLBACK;
      RETURN;
    END IF;

    -- anulacion de movimiento_Rechazo
    V_SQL := 'DELETE FROM T_MOVIMIENTO_RECHAZO
               WHERE ID_MOVIMIENTO IN (
                 SELECT M.ID_MOVIMIENTO
                   FROM T_MOVIMIENTOS M
                  WHERE M.FEC_MOVIMIENTO BETWEEN TRUNC(:FD) AND TRUNC(:FH+1) -1/24/60/60
                    AND M.ID_TIPO_MOVIMIENTO = 1
                    AND M.ID_ESTADO = 1
                    AND M.LIQUIDADO_SN IS NULL
                    AND M.ID_PROGRAMA = ' || I_PROGRAMA ||
                    ' AND NVL(M.ID_RESULTADO,0) != 4' || V_FILTROS ||
             ' ) ';

    DBMS_OUTPUT.PUT_LINE(V_SQL);

    EXECUTE IMMEDIATE V_SQL
      USING I_FECHA_DESDE, I_FECHA_HASTA;

    COMMIT;

  END SP_LIMPIAR_VALIDACION;

  /*********************************************************************************************
  * Validar Codigo de Autorizacion inexistente                                                 *
  *                                                                                            *
  * Boletos con c�digo de autorizacion no existente en la base de gobierno                     *
  *                                                                                            *
  * Validacion 87 - RECH387                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_AUTORIZACION_INEX(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS
  
  BEGIN
  
    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS T
               WHERE T.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND NOT EXISTS
               (SELECT 1
                        FROM MAASP_TUNI_TPTE.T_AUTORIZACIONES    A,
                             MAASP_TUNI_TPTE.T_TIPOS_SOLICITANTE S
                       WHERE T.ID_AUTORIZACION = A.ID_AUTORIZACION
                         AND A.ID_TIPO_SOLICITANTE = S.ID_TIPO_SOLICITANTE
                         AND T.ID_PROGRAMA = S.ID_PROGRAMA)) LOOP
    
      RETURN 0;
    
    END LOOP;
  
    RETURN 1;
  
  END;
  
  /*********************************************************************************************
  * Validar Autorizacion Vencida                                                               *
  *                                                                                            *
  * Boletos con con fecha de movimiento por fuera de la vigencia de la autorizacion            *
  *                                                                                            *
  * Validacion 21 - RECH321                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_AUTORIZACION_VIGEN(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS T
                   , T_AUTORIZACIONES A
               WHERE T.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND A.ID_AUTORIZACION = T.ID_AUTORIZACION
                 AND trunc(T.FEC_MOVIMIENTO) NOT BETWEEN trunc(A.FEC_DESDE) 
                                                     AND trunc(A.FEC_HASTA)  )
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_AUTORIZACION_VIGEN;
  
  /*********************************************************************************************
  * Validar Tipo de Linea                                                                      *
  *                                                                                            *
  * Boletos de administradora ATOS (urbano Capital) que no informen el tipo de linea o         *
  * informen uno inexistente                                                                   *
  *                                                                                            *
  * Validacion 22 - RECH322                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_TIPO_LINEA_ATOS(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS T
                   , T_AUTORIZACIONES A
               WHERE T.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND T.ID_ADMINISTRADORA = 3
                 AND (t.tipo_linea IS NULL OR 
                      T.TIPO_LINEA NOT IN ( SELECT ID_TIPO_BOLETO_URBANO 
                                              FROM T_TIPOS_BOLETO_URBANO )) )
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_TIPO_LINEA_ATOS;
  
  /*********************************************************************************************
  * Validar Autorizacion Inconsistente                                                         *
  *                                                                                            *
  * Boletos con autorizacion inconsistente en la base de gobierno                              *
  *                                                                                            *
  * Validacion 86 - RECH286                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_AUTORIZACION_INCONS(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

    V_ID_TIPO_EMPRESA      TRANSPORTE.T_TIPOS_EMPRESA.ID_TIPO_EMPRESA%TYPE;
    V_CANT_USO_X_DIA       MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_DIA%TYPE;
    V_CANT_USO_X_MES       MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_MES%TYPE;
    V_CANT_USO_X_DIA_SEM   MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_DIA_SEM%TYPE;
    V_CANT_USO_X_SEMANA    MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_SEMANA%TYPE;
    V_ID_DISTANCIA_EMPRESA MAASP_TUNI_TPTE.T_AUTORIZACIONES.ID_DISTANCIA%TYPE;

  BEGIN

   /* SELECT I.ID_TIPO_EMPRESA
          ,NVL(A.CANT_USO_X_DIA, 0)
          ,NVL(A.CANT_USO_X_MES, 0)
          ,TO_NUMBER(NVL(A.CANT_USO_X_DIA_SEM, 0))
          ,NVL(A.CANT_USO_X_SEMANA, 0)
          ,A.ID_DISTANCIA
      INTO V_ID_TIPO_EMPRESA
          ,V_CANT_USO_X_DIA
          ,V_CANT_USO_X_MES
          ,V_CANT_USO_X_DIA_SEM
          ,V_CANT_USO_X_SEMANA
          ,V_ID_DISTANCIA_EMPRESA
      FROM MAASP_TUNI_TPTE.T_MOVIMIENTOS M
          ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
         -- ,TRANSPORTE.T_TIPOS_EMPRESA E
          ,TRANSPORTE.T_EMPRESAS_X_TIPO I
     WHERE M.ID_AUTORIZACION = A.ID_AUTORIZACION
      AND A.ID_EMPRESA=I.ID_EMPRESA
      AND M.ID_TIPO_EMPRESA=I.ID_TIPO_EMPRESA
      AND M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
      AND M.ID_AUTORIZACION = A.ID_AUTORIZACION;*/
SELECT  m.ID_TIPO_EMPRESA
          ,
          NVL(A.CANT_USO_X_DIA, 0)
          ,NVL(A.CANT_USO_X_MES, 0)
          ,TO_NUMBER(NVL(A.CANT_USO_X_DIA_SEM, 0))
          ,NVL(A.CANT_USO_X_SEMANA, 0)
          ,A.ID_DISTANCIA
      INTO V_ID_TIPO_EMPRESA
          ,V_CANT_USO_X_DIA
          ,V_CANT_USO_X_MES
          ,V_CANT_USO_X_DIA_SEM
          ,V_CANT_USO_X_SEMANA
          ,V_ID_DISTANCIA_EMPRESA
      FROM MAASP_TUNI_TPTE.T_MOVIMIENTOS M
          ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
    WHERE M.ID_AUTORIZACION = A.ID_AUTORIZACION
        AND M.ID_MOVIMIENTO = I_ID_MOVIMIENTO;
    -- urbano capital
    IF V_ID_TIPO_EMPRESA = 4
    THEN

      -- usos x dia o usos x semana mutuamente excluyente
      -- usos x dia semana siempre deberia venir

      -- sin datos de dia de semana
      IF V_CANT_USO_X_DIA_SEM = 0
      THEN
        RETURN 0;
      END IF;

      -- usos por dia  y usos por semana ambos con datos
      IF V_CANT_USO_X_DIA != 0
         AND V_CANT_USO_X_SEMANA != 0
      THEN
        RETURN 0;
      END IF;

      -- usos por dia y usos por semana ambos sin datos
      IF V_CANT_USO_X_DIA = 0
         AND V_CANT_USO_X_SEMANA = 0
      THEN
        RETURN 0;
      END IF;

    END IF;

    -- urbano interior
    IF V_ID_TIPO_EMPRESA = 1
    THEN
      -- solo debe tener usos_x_dia_semana
      IF V_CANT_USO_X_DIA_SEM = 0
      THEN
        RETURN 0;
      END IF;

      IF V_CANT_USO_X_DIA != 0
         OR V_CANT_USO_X_SEMANA != 0
         OR V_CANT_USO_X_MES != 0
      THEN
        RETURN 0;
      END IF;

    END IF;

    -- interurbanos y diferenciales
    IF V_ID_TIPO_EMPRESA IN (2, 3)
    THEN

      -- media distancia (menos de 100km)
      IF V_ID_DISTANCIA_EMPRESA < 3
      THEN
        -- solo debe informar dia de semana
        IF V_CANT_USO_X_DIA_SEM = 0
        THEN
          RETURN 0;
        END IF;

        IF V_CANT_USO_X_DIA != 0
           OR V_CANT_USO_X_SEMANA != 0
           OR V_CANT_USO_X_MES != 0
        THEN
          RETURN 0;
        END IF;

      END IF;

      -- larga distancia (mas de 100km)
      IF V_ID_DISTANCIA_EMPRESA = 3
      THEN

        -- solo debe informar usos por mes
        IF V_CANT_USO_X_MES = 0
        THEN
          RETURN 0;
        END IF;

        IF V_CANT_USO_X_DIA != 0
           OR V_CANT_USO_X_SEMANA != 0
           OR V_CANT_USO_X_DIA_SEM != 0
        THEN
          RETURN 0;
        END IF;

      END IF;

    END IF;

    RETURN 1;

  END;

  /*********************************************************************************************
  * Validar Codigo de Empresa Autorizado Inexistente                                           *
  *                                                                                            *
  * Boletos con codigo de empresa coincidente con la autorizacion pero inexistente en la base  *
  * de gobierno (o autorizacion no existente)                                                  *
  *                                                                                            *
  * Validacion 18 - RECH318                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_EMPRESA_INEXISTENTE(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND A.ID_EMPRESA = M.ID_EMPRESA
                 AND NOT EXISTS (SELECT 1
                        FROM TRANSPORTE.T_EMPRESAS E
                       WHERE E.ID_EMPRESA = M.ID_EMPRESA))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_EMPRESA_INEXISTENTE;

  /*******************************************************************************************
  * Validar Empresa Fuera de Servicio                                                        *
  *                                                                                          *
  * Boletos con cuya fecha de movimiento sea posterior a la fecha de baja de actividad de la *
  * empresa informada                                                                        *
  *                                                                                          *
  * Validacion 74 - RECH374                                                                  *
  *******************************************************************************************/
  FUNCTION FU_VALIDAR_EMP_FUERA_SERVICIO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS         M
                    ,TRANSPORTE.T_EMPRESAS E
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_EMPRESA = E.ID_EMPRESA
                 AND M.FEC_MOVIMIENTO > NVL(E.FECHA_BAJA, M.FEC_MOVIMIENTO + 1))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_EMP_FUERA_SERVICIO;

  /*********************************************************************************************
  * Validar Cantidad de usos autorizados                                                       *
  *                                                                                            *
  * Boletos que excenden la cantidad de usos autorizados segun tipo de servicio                *
  *                                                                                            *
  * Validacion 7 - RECH207                                                                     *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_USOS_AUTORIZADOS(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

    V_ID_TIPO_EMPRESA       TRANSPORTE.T_TIPOS_EMPRESA.ID_TIPO_EMPRESA%TYPE;
    V_CANT_USO_X_DIA        MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_DIA%TYPE;
    V_CANT_USO_X_MES        MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_MES%TYPE;
    V_CANT_USO_X_DIA_SEM    MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_DIA_SEM%TYPE;
    V_CANT_USO_X_SEMANA     MAASP_TUNI_TPTE.T_AUTORIZACIONES.CANT_USO_X_SEMANA%TYPE;
    V_ID_DISTANCIA_EMPRESA  MAASP_TUNI_TPTE.T_AUTORIZACIONES.ID_DISTANCIA%TYPE;
    V_ID_AUTORIZACION       MAASP_TUNI_TPTE.T_AUTORIZACIONES.ID_AUTORIZACION%TYPE;
    V_FEC_MOVIMIENTO        T_MOVIMIENTOS.FEC_MOVIMIENTO%TYPE;
    V_ID_PROGRAMA           T_MOVIMIENTOS.ID_PROGRAMA%TYPE;
    V_NUMERO_DIA            INTEGER;
    V_NUMERO_DIA_PRIMER_USO INTEGER;
    V_CANT_AUTORIZADOS      INTEGER;
    V_CANT_USADOS           INTEGER;
    V_RANGO_SEMANA          INTEGER;

  BEGIN

    SELECT M.ID_TIPO_EMPRESA
          ,NVL(A.CANT_USO_X_DIA, 0)
          ,NVL(A.CANT_USO_X_MES, 0)
          ,NVL(A.CANT_USO_X_DIA_SEM, 0)
          ,NVL(A.CANT_USO_X_SEMANA, 0)
          ,A.ID_DISTANCIA
          ,DECODE(TRIM(TO_CHAR(M.FEC_MOVIMIENTO
                              ,'DAY'
                              ,'NLS_DATE_LANGUAGE = english'))
                 ,'MONDAY'
                 ,1
                 ,'TUESDAY'
                 ,2
                 ,'WEDNESDAY'
                 ,3
                 ,'THURSDAY'
                 ,4
                 ,'FRIDAY'
                 ,5
                 ,'SATURDAY'
                 ,6
                 ,'SUNDAY'
                 ,7) NUMERO_DIA
          ,A.ID_AUTORIZACION
  --      ,M.ID_AUTORIZACION
          ,M.FEC_MOVIMIENTO
          ,M.ID_PROGRAMA
      INTO V_ID_TIPO_EMPRESA
          ,V_CANT_USO_X_DIA
          ,V_CANT_USO_X_MES
          ,V_CANT_USO_X_DIA_SEM
          ,V_CANT_USO_X_SEMANA
          ,V_ID_DISTANCIA_EMPRESA
          ,V_NUMERO_DIA
          ,V_ID_AUTORIZACION
          ,V_FEC_MOVIMIENTO
          ,V_ID_PROGRAMA
     FROM MAASP_TUNI_TPTE.T_MOVIMIENTOS M
          ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A--,
   --       TRANSPORTE.T_TIPOS_EMPRESA E,
    --      TRANSPORTE.T_EMPRESAS_X_TIPO I
     WHERE M.ID_AUTORIZACION = A.ID_AUTORIZACION
      -- AND A.ID_EMPRESA=I.ID_EMPRESA
     --  AND M.ID_TIPO_EMPRESA=I.ID_TIPO_EMPRESA
       AND M.ID_MOVIMIENTO = I_ID_MOVIMIENTO;
    --   AND M.ID_AUTORIZACION = A.ID_AUTORIZACION;

    ---------
    -- BEG --
    ---------
    IF V_ID_PROGRAMA = 1 
    THEN
    
      -- urbano interior
      IF V_ID_TIPO_EMPRESA = 1
      THEN

        -- busco cuantos boletos tengo autorizados para hoy
        V_CANT_AUTORIZADOS := SUBSTR(V_CANT_USO_X_DIA_SEM, V_NUMERO_DIA, 1);

        -- busco cuantos boletos se usaron hoy inluido este
        SELECT COUNT(8)
          INTO V_CANT_USADOS
          FROM T_MOVIMIENTOS M
         WHERE M.ID_TIPO_MOVIMIENTO = 1
           AND M.ID_ESTADO = 1
           AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
           AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
           AND TRUNC(M.FEC_MOVIMIENTO) = TRUNC(V_FEC_MOVIMIENTO); -- del mismo dia

        -- si me paso rechazo
        IF V_CANT_USADOS > V_CANT_AUTORIZADOS
        THEN
          RETURN 0;
        END IF;

      END IF;

      -- interurbano y diferenciales
      IF V_ID_TIPO_EMPRESA IN (2, 3)
      THEN

        -- corta distancia (funciona igual que un urbano)
        IF V_ID_DISTANCIA_EMPRESA < 3
        THEN

          -- busco cuantos boletos tengo autorizados para hoy
          V_CANT_AUTORIZADOS := SUBSTR(V_CANT_USO_X_DIA_SEM, V_NUMERO_DIA, 1);

          -- busco cuantos boletos se usaron hoy inluido este
          SELECT COUNT(8)
            INTO V_CANT_USADOS
            FROM T_MOVIMIENTOS M
           WHERE M.ID_TIPO_MOVIMIENTO = 1
             AND M.ID_ESTADO = 1
             AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
             AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores

             AND TRUNC(M.FEC_MOVIMIENTO) = TRUNC(V_FEC_MOVIMIENTO); -- del mismo dia

          -- si me paso rechazo
          IF V_CANT_USADOS > V_CANT_AUTORIZADOS
          THEN
            RETURN 0;
          END IF;

        ELSE
          -- larga distancia

          -- controlo los usos por mes
          -- busco cuantos boletos se usaron este mes inluido este
          SELECT COUNT(8)
            INTO V_CANT_USADOS
            FROM T_MOVIMIENTOS M
           WHERE M.ID_TIPO_MOVIMIENTO = 1
             AND M.ID_ESTADO = 1
             AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
             AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
             AND TRUNC(M.FEC_MOVIMIENTO, 'month') =
                 TRUNC(V_FEC_MOVIMIENTO, 'month'); -- del mismo mes

          -- si me paso rechazo
          IF V_CANT_USADOS > V_CANT_USO_X_MES
          THEN
            RETURN 0;
          END IF;

        END IF;

      END IF;

      -- urbano capital
      IF V_ID_TIPO_EMPRESA = 4
      THEN
        
        -- determino la fecha del primer movimiento
        select DECODE(TRIM(TO_CHAR(min(fec_movimiento)
                                ,'DAY'
                                ,'NLS_DATE_LANGUAGE = english'))
                   ,'MONDAY'
                   ,1
                   ,'TUESDAY'
                   ,2
                   ,'WEDNESDAY'
                   ,3
                   ,'THURSDAY'
                   ,4
                   ,'FRIDAY'
                   ,5
                   ,'SATURDAY'
                   ,6
                   ,'SUNDAY'
                   ,7)
          into V_NUMERO_DIA_PRIMER_USO
          from t_movimientos
         where id_tipo_movimiento = 1
           and id_estado = 1
           and id_autorizacion = V_ID_AUTORIZACION;
        
        -- determino el rango de dias a buscar
        IF V_NUMERO_DIA >= V_NUMERO_DIA_PRIMER_USO THEN
        
          V_RANGO_SEMANA := V_NUMERO_DIA - V_NUMERO_DIA_PRIMER_USO;
        
        ELSE
          
          V_RANGO_SEMANA := V_NUMERO_DIA + 7 - V_NUMERO_DIA_PRIMER_USO;
          
        END IF;
        
        -- busco cuantos boletos se usaron esta semana (definida por primer uso)
        SELECT COUNT(8)
          INTO V_CANT_USADOS
          FROM T_MOVIMIENTOS M
         WHERE M.ID_TIPO_MOVIMIENTO = 1
           AND M.ID_ESTADO = 1
           AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
           AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
           AND TRUNC(M.FEC_MOVIMIENTO) between TRUNC(M.FEC_MOVIMIENTO) - v_rango_semana
                                           and TRUNC(M.FEC_MOVIMIENTO) - v_rango_semana + 7;
        
        IF V_CANT_USADOS > V_CANT_USO_X_SEMANA THEN
          
          RETURN 0;
          
        END IF;
        
      END IF;
   
    ---------
    -- BOS --
    ---------
    ELSIF V_ID_PROGRAMA = 2
    THEN
      
      -- URBANO
      IF V_ID_TIPO_EMPRESA = 1
      THEN
      
        -- SI TIENE USOS POR DIA
        IF V_CANT_USO_X_DIA != 0 
        THEN
          
          -- cuento cuantos boletos tengo para este dia
          SELECT COUNT(8)
            INTO V_CANT_USADOS
            FROM T_MOVIMIENTOS M
           WHERE M.ID_TIPO_MOVIMIENTO = 1
             AND M.ID_ESTADO = 1
             AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
             AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
             AND TRUNC(M.FEC_MOVIMIENTO) = TRUNC(V_FEC_MOVIMIENTO);
          
          -- controlo cantidad de usados
          IF V_CANT_USADOS > V_CANT_USO_X_DIA
          THEN 
            RETURN 0;
          END IF; 
          
          -- controlo que pueda viajar ese dia de la semana
          IF SUBSTR(V_CANT_USO_X_DIA_SEM, V_NUMERO_DIA, 1) = 0 
          THEN
            RETURN 0;
          END IF;
        
        -- SI TIENE USOS POR MES
        ELSIF V_CANT_USO_X_MES != 0 
        THEN
        
          -- cuento cuantos boletos tengo para este mes
          SELECT COUNT(8)
            INTO V_CANT_USADOS
            FROM T_MOVIMIENTOS M
           WHERE M.ID_TIPO_MOVIMIENTO = 1
             AND M.ID_ESTADO = 1
             AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
             AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
             AND TRUNC(M.FEC_MOVIMIENTO,'month') = TRUNC(V_FEC_MOVIMIENTO,'month');
          
          -- controlo cantidad de usados
          IF V_CANT_USADOS > V_CANT_USO_X_DIA
          THEN 
            RETURN 0;
          END IF; 
          
          -- controlo que pueda viajar ese dia de la semana
          IF SUBSTR(V_CANT_USO_X_DIA_SEM, V_NUMERO_DIA, 1) = 0 
          THEN
            RETURN 0;
          END IF;
        
        -- SINO CONTROLO USOS POR DIA DE SEMANA
        ELSE
          
          -- busco cuantos boletos tengo autorizados para hoy
          V_CANT_AUTORIZADOS := SUBSTR(V_CANT_USO_X_DIA_SEM, V_NUMERO_DIA, 1);

          -- busco cuantos boletos se usaron hoy inluido este
          SELECT COUNT(8)
            INTO V_CANT_USADOS
            FROM T_MOVIMIENTOS M
           WHERE M.ID_TIPO_MOVIMIENTO = 1
             AND M.ID_ESTADO = 1
             AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
             AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
             AND TRUNC(M.FEC_MOVIMIENTO) = TRUNC(V_FEC_MOVIMIENTO); -- del mismo dia

          -- si me paso rechazo
          IF V_CANT_USADOS > V_CANT_AUTORIZADOS
          THEN
            RETURN 0;
          END IF;
        
        END IF;
      
      -- INTERURBANO/DIFERENCIAL
      ELSIF V_ID_TIPO_EMPRESA IN (2,3)
      THEN
        
        -- busco cuantos boletos tengo autorizados para hoy
        V_CANT_AUTORIZADOS := SUBSTR(V_CANT_USO_X_DIA_SEM, V_NUMERO_DIA, 1);

        -- busco cuantos boletos se usaron hoy inluido este
        SELECT COUNT(8)
          INTO V_CANT_USADOS
          FROM T_MOVIMIENTOS M
         WHERE M.ID_TIPO_MOVIMIENTO = 1
           AND M.ID_ESTADO = 1
           AND M.ID_AUTORIZACION = V_ID_AUTORIZACION -- para esta autorizacion
           AND M.ID_MOVIMIENTO <= I_ID_MOVIMIENTO -- contando este boleto y anteriores
           AND TRUNC(M.FEC_MOVIMIENTO) = TRUNC(V_FEC_MOVIMIENTO); -- del mismo dia

        -- si me paso rechazo
        IF V_CANT_USADOS > V_CANT_AUTORIZADOS
        THEN
          RETURN 0;
        END IF;
      
      END IF;
    
    
    ELSE
      
      -- NO ESTA SOPORTADO ESTE PROGRAMA
      RETURN 0;
      
    END IF;
   
    RETURN 1;

  END FU_VALIDAR_USOS_AUTORIZADOS;

  /****************************************************************
  * Validar Feriado en distancias menores a 100km                 *
  *                                                               *
  * Boletos que viajaron dia feriado distancias menores a 100km   *
  *                                                               *
  * Validacion 9 - RECH309                                        *
  ****************************************************************/
  FUNCTION FU_VALIDAR_FERIADO_MENOR_100(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    -- ## falta agregar feriados locales

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,T_COMUNES.VT_FERIADOS           F
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND TRUNC(FEC_MOVIMIENTO) = F.FECHA
                 AND F.ID_TIPO_FERIADO = '01' -- feriado nacional
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND A.ID_DISTANCIA != 3) --menor a 100km
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_FERIADO_MENOR_100;
  
  /*********************************************************
  * Validar Feriado general                                *
  *                                                        *
  * Boletos que viajaron dia feriado cualquier distancia   *
  *                                                        *
  * Validacion 19 - RECH319                                *
  *********************************************************/
  FUNCTION FU_VALIDAR_FERIADO_GENERAL(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,T_COMUNES.VT_FERIADOS           F
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND TRUNC(FEC_MOVIMIENTO) = F.FECHA
                 AND F.ID_TIPO_FERIADO = '01') -- feriado nacional
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_FERIADO_GENERAL;
  
  /**********************************************************
  * Validar Feriado local                                   *
  *                                                         *
  * Boletos urbanos que viajaron un dia feriado local en la * 
  * localidad que opera la empresa                          *
  *                                                         *
  * Validacion 22 - RECH222                                 *
  **********************************************************/
  FUNCTION FU_VALIDAR_FERIADO_LOCAL(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,T_COMUNES.VT_FERIADOS           F
                    ,TRANSPORTE.T_EMPRESAS           E
                    ,TRANSPORTE.T_EMPRESAS_x_TIPO    T
               WHERE M.ID_MOVIMIENTO       = I_ID_MOVIMIENTO
                 AND M.ID_EMPRESA          = E.ID_EMPRESA
                 AND T.ID_EMPRESA          = E.ID_EMPRESA
                 AND T.ID_TIPO_EMPRESA     IN (1,4)
                 AND TRUNC(FEC_MOVIMIENTO) = F.FECHA
                 AND F.ID_TIPO_FERIADO     = '02'
                 AND F.ID_LOCALIDAD        = E.ID_LOCALIDAD ) -- feriado local
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_FERIADO_LOCAL;
  
  /****************************************************************
  * Validar Feriado en distancias mayores a 100km                 *
  *                                                               *
  * Boletos que viajaron dia feriado distancias mayores a 100km   *
  *                                                               *
  * Validacion 9 - RECH209                                        *
  ****************************************************************/
  FUNCTION FU_VALIDAR_FERIADO_MAYOR_100(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    -- ## falta agregar feriados locales

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,T_COMUNES.VT_FERIADOS           F
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND TRUNC(FEC_MOVIMIENTO) = F.FECHA
                 AND F.ID_TIPO_FERIADO = '01' -- feriado nacional
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND A.ID_DISTANCIA = 3) --mayor a 100km
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_FERIADO_MAYOR_100;

  /*********************************************************************************************
  * Validar Verificaciones Duplicadas                                                          *
  *                                                                                            *
  * Boletos con un boleto anterior preexistiente con el mismo codigo de verificacion y empresa *
  *                                                                                            *
  * Validacion 10 - RECH310                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_VERIFICACION_DUP(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

    V_NRO_VERSION   T_MOVIMIENTOS.NRO_VERSION%TYPE;

  BEGIN

    -- verificar si existe un boleto anterior con mismo codigo de verificacion y empresa informada

    SELECT NRO_VERSION
      INTO V_NRO_VERSION
      FROM T_MOVIMIENTOS
     WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO;

    -- no controlo duplicados para las rectificativas
    IF V_NRO_VERSION != 1 THEN
      RETURN 1;
    END IF;

    -- busco boletos anteriores al primero de la cadena de rectificacion
    -- que esten vigentes o hayan sido rectificados
    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS T
               WHERE T.ID_MOVIMIENTO < I_ID_MOVIMIENTO
                 AND T.ID_TIPO_MOVIMIENTO = 1
                 AND T.ID_ESTADO IN (1, 2)
                 AND (T.CODIGO_VERIFICACION, T.ID_GRUPO) IN
                     (SELECT T2.CODIGO_VERIFICACION
                            ,T2.ID_GRUPO
                        FROM T_MOVIMIENTOS T2
                       WHERE T2.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                       AND NVL(T.ID_GRUPO, -1) != NVL(T2.ID_GRUPO, -2)))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_VERIFICACION_DUP;

  /******************************************************************************************************
  * Validar Boletos Duplicados                                                                          *
  *                                                                                                     *
  * Boletos con un boleto anterior preexistiente con la misma autorizaci�n y fecha exacta de movimiento *
  *                                                                                                     *
  * Validacion 11 - RECH311                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_BOLETO_DUP(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)

   RETURN INTEGER IS

    V_ID_AUTORIZACION T_MOVIMIENTOS.ID_AUTORIZACION%TYPE;
    v_nro_version t_movimientos.nro_version%type;

  BEGIN

    -- verificar que se repita fecha_boleto y autorizacion
    -- se verifica que el boleto sea posterior para no rebotar el primero en caso de
    -- validar posterior a la extracci�n del repetido

    SELECT ID_AUTORIZACION
         , NRO_VERSION
      INTO V_ID_AUTORIZACION
         , v_nro_version
      FROM T_MOVIMIENTOS
     WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO;

    -- si es una rectificativa no controlo duplicados
    if v_nro_version != 1 then
      return 1;
    end if;

    -- busco boletos anteriores al primero de la cadena de rectificacion
    -- que esten vigentes o hayan sido rectificados
    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS T
               WHERE T.ID_AUTORIZACION = V_ID_AUTORIZACION -- misma autorizacion
                 AND T.ID_MOVIMIENTO < I_ID_MOVIMIENTO -- boleto posterior
                 AND T.ID_TIPO_MOVIMIENTO = 1 -- uso
                 AND T.ID_ESTADO IN (1, 2)
                 AND TO_CHAR(T.FEC_MOVIMIENTO, 'DD/MM/YYYY HH24:MI') = -- misma fecha en minutos
                     (SELECT TO_CHAR(T2.FEC_MOVIMIENTO, 'DD/MM/YYYY HH24:MI')
                        FROM T_MOVIMIENTOS T2
                       WHERE T2.ID_MOVIMIENTO = I_ID_MOVIMIENTO))
    LOOP

      -- existe un repetido
      RETURN 0;

    END LOOP;

    -- no existe repetido
    RETURN 1;

  END FU_VALIDAR_BOLETO_DUP;

  /*********************************************************************************************
  * Validar Codigo de Empresa no informado o negativo                                          *
  *                                                                                            *
  * Boletos sin c�digo de empresa o con codigo menor a uno                                     *
  *                                                                                            *
  * Validacion 90 - RECH390                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_EMPRESA_NULA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS
               WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND NVL(ID_EMPRESA, 0) < 1)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_EMPRESA_NULA;

  /*********************************************************************************************
  * Validar Codigo de Autorizacion no informado o negativo                                     *
  *                                                                                            *
  * Boletos sin c�digo de autorizacion o con codigo menor a uno                                *
  *                                                                                            *
  * Validacion 88 - RECH388                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_AUTORIZACION_NULA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS
               WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND NVL(ID_AUTORIZACION, 0) < 1)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_AUTORIZACION_NULA;

  /*********************************************************************************************
  * Validar CUIL Nulo o Malformado                                                             *
  *                                                                                            *
  * Boletos sin CUIL o con CUIL de longitud distinta de 11 o no comienza con 20,23,24 o 27     *
  *                                                                                            *
  * Validacion 89 - RECH389                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_CUIL_MALFORMADO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS
               WHERE ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND (CUIL IS NULL OR LENGTH(CUIL) != 11 OR
                     SUBSTR(CUIL, 1, 2) NOT IN ('20', '23', '24', '27')))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_CUIL_MALFORMADO;

  /*********************************************************************************************
  * Validar CUIL contra la autorizacion                                                        *
  *                                                                                            *
  * Boletos cuyo CUIL no coincide con el de la autorizacion                                    *
  * (tambien se rechazan boletos sin CUIL o autorizaciones sin CUIL                            *
  *                                                                                            *
  * Validacion 96 - RECH396                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_CUIL_AUTORIZADO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND A.ID_AUTORIZACION(+) = M.ID_AUTORIZACION
                 AND (M.CUIL != A.CUIL OR M.CUIL IS NULL OR A.CUIL IS NULL))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_CUIL_AUTORIZADO;

  /*********************************************************************************************
  * Validar Tipo de solicitante nulo                                                           *
  *                                                                                            *
  * Boletos cuyo tipo de solicitante en la autorizacion es nulo                                *
  *                                                                                            *
  * Validacion 17 - RECH317                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_TIPO_SOL_NULO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND A.ID_TIPO_SOLICITANTE IS NULL)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_TIPO_SOL_NULO;

  /*********************************************************************************************
  * Validar boleto urbano contra empresa no urbana                                             *
  *                                                                                            *
  * Boletos cuyo tipo de empresa es urbano del interior pero la empresa esta cargada con otro  *
  * tipo                                                                                       *
  *                                                                                            *
  * Validacion 92 - RECH392                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_URB_EMP_NOURB(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS M
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_TIPO_EMPRESA = 1 --Urbano del Interior
                 AND EXISTS (SELECT 1
                        FROM TRANSPORTE.T_EMPRESAS_X_TIPO E
                       WHERE M.ID_EMPRESA = E.ID_EMPRESA
                         AND E.ID_TIPO_EMPRESA != 1 --Diferente a Urbano del Interior
                      ))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_URB_EMP_NOURB;

  /*********************************************************************************************
  * Validar boleto urbano contra autorizacion no urbana                                        *
  *                                                                                            *
  * Boletos cuyo tipo de empresa es urbano del interior pero la autorizacion cargada con otro  *
  * tipo                                                                                       *
  *                                                                                            *
  * Validacion 81 - RECH381                                                                    *
  *********************************************************************************************/
  FUNCTION FU_VALIDAR_URB_AUT_NOURB(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS M
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_TIPO_EMPRESA = 1 --Urbano del Interior
                 AND EXISTS (SELECT 1
                   FROM MAASP_TUNI_TPTE.T_AUTORIZACIONES A,
--                        TRANSPORTE.T_TIPOS_EMPRESA E,
                        TRANSPORTE.T_EMPRESAS_X_TIPO I
                  WHERE M.ID_AUTORIZACION = A.ID_AUTORIZACION
                    AND M.ID_TIPO_EMPRESA=I.ID_TIPO_EMPRESA
                    AND A.ID_EMPRESA=I.ID_EMPRESA
                    AND I.ID_TIPO_EMPRESA != 1 --Diferente de Urbano del Interior
                      ))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_URB_AUT_NOURB;

  /******************************************************************************************************
  * Validar Distinta Empresa Grupo Urbano                                                               *
  *                                                                                                     *
  * Boletos con distinta empresa en la autorizacion, del mismo grupo de empresas pero ambas empresas    *
  * son urbanas                                                                                         *
  *                                                                                                     *
  * Validacion 55 - RECH355                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_DIS_EMP_GRUPO_URB(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    TA
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    TB
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_AUTORIZACION = A.ID_AUTORIZACION
                 AND M.ID_EMPRESA != A.ID_EMPRESA -- distinta empresa
                 AND A.ID_EMPRESA = TA.ID_EMPRESA
                 AND M.ID_EMPRESA = TB.ID_EMPRESA
                /* AND M.ID_TIPO_EMPRESA = 1 -- ambas urbanas
                 AND TA.ID_TIPO_EMPRESA = 1 -- ambas urbanas
                 AND TB.ID_TIPO_EMPRESA=1 -- ambas urbanas*/
                 AND TA.ID_GRUPO != TB.ID_GRUPO -- mismo grupo
              )
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END;

  /******************************************************************************************************
  * Validar Distinta Empresa distinto Grupo                                                             *
  *                                                                                                     *
  * Boletos con distinta empresa en la autorizacion y distinto grupo (o sin autorizacion)               *
  *                                                                                                     *
  * Validacion 59 - RECH359                                                                   *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_DIS_EMP_DIS_GRUPO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)

   RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    TA
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    TB
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_AUTORIZACION = A.ID_AUTORIZACION
                 AND M.ID_EMPRESA != A.ID_EMPRESA -- distinta empresa
                 AND A.ID_EMPRESA = TA.ID_EMPRESA
                 AND M.ID_EMPRESA = TB.ID_EMPRESA
                 AND NVL(TA.ID_GRUPO, -1) != NVL(TB.ID_GRUPO, -2) -- distinto grupo
              )
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_DIS_EMP_DIS_GRUPO;

  /******************************************************************************************************
  * Validar Distinta Empresa distinto Grupo Informado                                                   *
  *                                                                                                     *
  *                                                                                                     *
  * Validacion 70 - RECH370                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_MIS_EMP_DIS_GRP_INF(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    TA
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    TB
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_AUTORIZACION = A.ID_AUTORIZACION -- esta autorizada
                 AND A.ID_EMPRESA = TA.ID_EMPRESA
                 AND M.ID_EMPRESA = TB.ID_EMPRESA
                 AND M.ID_EMPRESA != A.ID_EMPRESA -- distinta empresa informada a la autorizada
                 AND TB.ID_GRUPO = TA.ID_GRUPO -- grupo de empresa informada igual grupo autorizado
                 AND M.ID_TIPO_EMPRESA != TA.ID_TIPO_EMPRESA -- servicio informado distinto al servicio autorizado
              )
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_MIS_EMP_DIS_GRP_INF;

  /******************************************************************************************************
  * Validar Horario Autorizado                                                                          *
  *                                                                                                     *
  * Boletos con hora por fuera del rango horario indicado en la autorizacion                            *
  *                                                                                                     *
  * Validacion 08 - RECH208                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_FRANJA_HORARIA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS MV
               INNER JOIN MAASP_TUNI_TPTE.T_AUTORIZACIONES A
                  ON (MV.ID_AUTORIZACION = A.ID_AUTORIZACION)
               WHERE MV.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND (CASE
                       WHEN TO_CHAR(MV.FEC_MOVIMIENTO, 'HH24:MI') = '00:00' THEN
                        '24:00'
                       ELSE
                        TO_CHAR(MV.FEC_MOVIMIENTO, 'HH24:MI')
                     END < A.INICIO_FRANJA_HORARIA AND
                     CASE
                       WHEN TO_CHAR(MV.FEC_MOVIMIENTO, 'HH24:MI') = '00:00' THEN
                        '24:00'
                       ELSE
                        TO_CHAR(MV.FEC_MOVIMIENTO, 'HH24:MI')
                     END > A.FINAL_FRANJA_HORARIA))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_FRANJA_HORARIA;

  /******************************************************************************************************
  * Validar Lista Negra                                                                                 *
  *                                                                                                     *
  * Boletos con tarjeta en lista negra                                                                  *
  *                                                                                                     *
  * Validacion 16 - RECH216                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_LISTA_NEGRA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS MV
               WHERE MV.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND EXISTS
               (SELECT 1
                        FROM ABONO_ESTUDIANTIL.VT_LISTA_NEGRA_BEG LN
                       WHERE LN.UIDS = MV.UID_TARJETA
                         AND LN.FECHA + 3 < TRUNC(MV.FEC_MOVIMIENTO)))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_LISTA_NEGRA;

  /******************************************************************************************************
  * Validar Interurbano con Autorizacion Urbana                                                         *
  *                                                                                                     *
  * Boletos con tipo de empresa interurbana pero autorizacion urbana                                    *
  *                                                                                                     *
  * Validacion 80 - RECH280                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_INTER_AUT_URBANO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM MAASP_TUNI_TPTE.T_MOVIMIENTOS MV
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A,
                     TRANSPORTE.T_EMPRESAS_X_TIPO I
               WHERE MV.ID_AUTORIZACION = A.ID_AUTORIZACION
                AND A.ID_EMPRESA=I.ID_EMPRESA
                AND MV.ID_TIPO_EMPRESA=I.ID_TIPO_EMPRESA
                 AND MV.ID_TIPO_EMPRESA = 2 --Interurbano
                 AND I.ID_TIPO_EMPRESA =1 --Urbano del Interior
                 AND MV.ID_MOVIMIENTO = I_ID_MOVIMIENTO)

    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_INTER_AUT_URBANO;

  /******************************************************************************************************
  * Validar Interurbano con Empresa Urbana                                                              *
  *                                                                                                     *
  * Boletos con tipo de empresa interurbana pero empresa urbana en base de gobierno                     *
  *                                                                                                     *
  * Validacion 91 - RECH291                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_INTER_EM_URBANO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS MV
               INNER JOIN TRANSPORTE.T_EMPRESAS_X_TIPO EM
                  ON (MV.ID_EMPRESA = EM.ID_EMPRESA)
               WHERE MV.ID_TIPO_EMPRESA = 2 --Interurbano
                 AND EM.ID_TIPO_EMPRESA = 1 --Urbano del Interior
                 AND MV.ID_MOVIMIENTO = I_ID_MOVIMIENTO)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_INTER_EM_URBANO;

  /******************************************************************************************************
  * Validar Interurbano con Empresa Urbana                                                              *
  *                                                                                                     *
  * Boletos con empresa no autorizada, del mismo grupo pero boleto interurbano y empresa urbana         *
  *                                                                                                     *
  * Validacion 57 - RECH357                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_SERV_INTER(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                 FROM MAASP_TUNI_TPTE.T_MOVIMIENTOS MV
                     ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A,
                      TRANSPORTE.T_EMPRESAS_X_TIPO EA,
                      TRANSPORTE.T_EMPRESAS_X_TIPO EM
     WHERE MV.ID_AUTORIZACION = A.ID_AUTORIZACION
       AND MV.ID_EMPRESA = EM.ID_EMPRESA
       AND A.ID_EMPRESA = EA.ID_EMPRESA
       AND NVL(MV.ID_EMPRESA, -1) != NVL(A.ID_EMPRESA, -2)
       AND NVL(EM.ID_GRUPO, -1) = NVL(EA.ID_GRUPO, -2)
       AND NVL(MV.ID_TIPO_EMPRESA, -1) IN (2, 3)
       AND EA.ID_TIPO_EMPRESA = 1
       AND MV.ID_MOVIMIENTO = I_ID_MOVIMIENTO)


    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_SERV_INTER;

  /******************************************************************************************************
  * Validar Urbano con Empresa InterUrbana                                                              *
  *                                                                                                     *
  * Boletos con empresa no autorizada, del mismo grupo pero boleto urbano y empresa interurbana         *
  *                                                                                                     *
  * Validacion 58 - RECH358                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_SERV_URBANO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   MV
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    EM
                    ,TRANSPORTE.T_EMPRESAS_X_TIPO    EA
               WHERE MV.ID_AUTORIZACION = A.ID_AUTORIZACION
                 AND MV.ID_EMPRESA = EM.ID_EMPRESA
                 AND A.ID_EMPRESA = EA.ID_EMPRESA
                 AND NVL(MV.ID_EMPRESA, -1) != NVL(A.ID_EMPRESA, -2)
                 AND NVL(EM.ID_GRUPO, -1) = NVL(EA.ID_GRUPO, -2)
                 AND NVL(MV.ID_TIPO_EMPRESA, -1) = 1
                 AND NVL(EA.ID_TIPO_EMPRESA, -1) IN (2, 3)
                 AND MV.ID_MOVIMIENTO = I_ID_MOVIMIENTO)

    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_SERV_URBANO;

  /******************************************************************************************************
  * Validar Tarjeta                                                                                     *
  *                                                                                                     *
  * Boletos con tarjeta invalida                                                                        *
  *                                                                                                     *
  * Validacion 13 - RECH213                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_TARJETA(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    -- si la tarjeta esta en la solicitud esta correcta
    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                          M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES        A
                    ,MAASP_TUNI_TPTE.T_TARJETAS_AUTORIZACION S
                    ,MAASP_TUNI_TPTE.T_TARJETAS T
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND S.ID_AUTORIZACION = A.ID_AUTORIZACION
                 AND S.ID_TARJETA=T.ID_TARJETA
                 AND M.UID_TARJETA = T.UIDS
                 AND M.FEC_MOVIMIENTO BETWEEN S.FEC_HASTA AND
                     NVL(S.FEC_HASTA, M.FEC_MOVIMIENTO))
    LOOP

      RETURN 1; -- solo este caso es al revez del resto

    END LOOP;

    RETURN 0;

  END FU_VALIDAR_TARJETA;

  /******************************************************************************************************
  * Validar Origen/Destino Nulo                                                                         *
  *                                                                                                     *
  * Boletos no urbanos con origen o destino nulo                                                        *
  *                                                                                                     *
  * Validacion 15 - RECH215                                                                             *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_ORI_DES_NULO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS M
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_TIPO_EMPRESA IN (2, 3) -- solo valida para interurbanos y diferenciales
                 AND (NVL(M.ID_ORIGEN, 0) < 1 OR NVL(M.ID_DESTINO, 0) < 1))
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_ORI_DES_NULO;

  /******************************************************************************************************
  * Validar Tramo No Autorizado                                                                         *
  *                                                                                                     *
  * Boletos con Origen/Destino (o invertido) distinto al autorizado                                     *
  *                                                                                                     *
  * Validacion 5 - RECH205                                                                              *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_TRAMO_NO_AUT(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND M.ID_TIPO_EMPRESA IN (2, 3) -- solo valida para interurbanos y diferenciales
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND NOT
                      (M.ID_ORIGEN = A.ID_ORIGEN OR M.ID_ORIGEN = A.ID_DESTINO OR
                      M.ID_DESTINO = A.ID_DESTINO OR M.ID_DESTINO = A.ID_ORIGEN)) -- si no coincide el destino o el origen (o el inverso)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_TRAMO_NO_AUT;

  /******************************************************************************************************
  * Validar Fuera de Trimestre                                                                          *
  *                                                                                                     *
  * Boletos con fecha por fuera del vencimiento del tramo definido en la autorizacion                   *
  *                                                                                                     *
  * Validacion 6 - RECH206                                                                              *
  ******************************************************************************************************/
  FUNCTION FU_VALIDAR_FUERA_TRIMESTRE(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS                   M
                    ,MAASP_TUNI_TPTE.T_AUTORIZACIONES A
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
                 AND M.ID_ADMINISTRADORA != 1 -- excluir micronauta ##
                 AND TRUNC(M.FEC_MOVIMIENTO) > A.FEC_HASTA)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_FUERA_TRIMESTRE;

  /***********************************************************
  * Validar Periodos de Receso                               *
  *                                                          *
  * Boletos con fecha dentro de un per�odo de receso escolar *
  *                                                          *
  * Validacion 6 - RECH206                                   *
  ***********************************************************/
  FUNCTION FU_VALIDAR_RECESO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN INTEGER IS

  BEGIN

    FOR R IN (SELECT 1
                FROM T_MOVIMIENTOS     M
                    ,T_PERIODOS_RECESO R
               WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
                 AND TRUNC(FEC_MOVIMIENTO) BETWEEN R.FEC_DESDE AND R.FEC_HASTA)
    LOOP

      RETURN 0;

    END LOOP;

    RETURN 1;

  END FU_VALIDAR_RECESO;

  /*****************************************************************************************************
  * Aprobar Boletos por Lote                                                                           *
  *                                                                                                    *
  * Genera una operacion nueva tipo AP, aprueba (pasa de estado 2 a 4) los boletos resultantes por los *
  * filtros y los vincula a la operacion generada                                                      *
  *****************************************************************************************************/
  PROCEDURE SP_APROBAR_BOLETOS(I_USUARIO                   IN T_OPERACIONES.USUARIO_OPERACION%TYPE
                              ,I_OBSERVACIONES             IN T_OPERACIONES.OBSERVACIONES%TYPE DEFAULT NULL
                              ,I_FEC_MOVIMIENTO_DESDE      IN T_MOVIMIENTOS.FEC_MOVIMIENTO%TYPE
                              ,I_FEC_MOVIMIENTO_HASTA      IN T_MOVIMIENTOS.FEC_MOVIMIENTO%TYPE
                              ,I_CUIL                      IN T_MOVIMIENTOS.CUIL%TYPE DEFAULT NULL
                              ,I_LISTA_ID_ORIGEN           IN VARCHAR2 DEFAULT NULL
                              ,I_LISTA_ID_DESTINO          IN VARCHAR2 DEFAULT NULL
                              ,I_LISTA_ID_EMPRESA          IN VARCHAR2 DEFAULT NULL
                              ,I_LISTA_ID_ADMINISTRADORA   IN VARCHAR2 DEFAULT NULL
                              ,I_LISTA_ID_TIPO_EMPRESA     IN VARCHAR2 DEFAULT NULL
                              ,I_LISTA_ID_TIPO_SOLICITANTE IN VARCHAR2 DEFAULT NULL
                              ,I_LISTA_ID_VALIDACIONES     IN VARCHAR2 DEFAULT NULL) IS

    V_ID_OPERACION  NUMBER;
    V_SQL           VARCHAR2(10000);
    V_CUR           SYS_REFCURSOR;
    V_ID_MOVIMIENTO T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE;

  BEGIN

    -- controles de parametros
    IF I_USUARIO IS NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'Debe indicar un usuario para el uso de esta funcionalidad.');
    END IF;

    IF I_FEC_MOVIMIENTO_DESDE IS NULL
       OR I_FEC_MOVIMIENTO_HASTA IS NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'Los par�metros de rangos de fecha son ambos obligatorios.');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_ORIGEN, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_ORIGEN IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_ORIGEN tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_DESTINO, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_DESTINO IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_DESTINO tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_EMPRESA, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_EMPRESA IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_EMPRESA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_ADMINISTRADORA, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_ADMINISTRADORA IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_ADMINISTRADORA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_TIPO_EMPRESA, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_TIPO_EMPRESA IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_TIPO_EMPRESA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_TIPO_SOLICITANTE, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_TIPO_SOLICITANTE IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_TIPO_SOLICITANTE tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    IF NOT REGEXP_LIKE(I_LISTA_ID_VALIDACIONES, '^(\d+(,\d+)*)?$')
       AND I_LISTA_ID_VALIDACIONES IS NOT NULL
    THEN
      RAISE_APPLICATION_ERROR(-20001
                             ,'El parametro i_LISTA_ID_MOTIVOS_RECHAZO tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;

    -- genera el nuevo lote de operacion
    V_ID_OPERACION := SEQ_OPERACIONES.NEXTVAL;

    INSERT INTO T_OPERACIONES
      (ID_OPERACION
      ,ID_TIPO_OPERACION
      ,USUARIO_OPERACION
      ,FEC_OPERACION
      ,OBSERVACIONES)
    VALUES
      (V_ID_OPERACION
      ,'AP'
      ,I_USUARIO
      ,SYSDATE
      ,I_OBSERVACIONES);

    COMMIT;

    V_SQL := '
    SELECT M.ID_MOVIMIENTO
      FROM T_MOVIMIENTOS M
     WHERE M.FEC_MOVIMIENTO BETWEEN trunc(:FD) AND trunc(:FH+1) - 1/24/60/60
       AND M.ID_TIPO_MOVIMIENTO = 1
       AND M.ID_ESTADO = 1
       AND M.ID_RESULTADO = 2';

    -- agrego los filtros
    IF I_CUIL IS NOT NULL
    THEN
      V_SQL := V_SQL || ' AND M.CUIL = ''' || I_CUIL || ''' ';
    END IF;

    IF I_LISTA_ID_ORIGEN IS NOT NULL
    THEN
      V_SQL := V_SQL || ' AND M.ID_ORIGEN IN(' || I_LISTA_ID_ORIGEN || ') ';
    END IF;

    IF I_LISTA_ID_DESTINO IS NOT NULL
    THEN
      V_SQL := V_SQL || ' AND M.ID_DESTINO IN(' || I_LISTA_ID_DESTINO || ') ';
    END IF;

    IF I_LISTA_ID_EMPRESA IS NOT NULL
    THEN
      V_SQL := V_SQL || ' AND M.ID_EMPRESA IN (' || I_LISTA_ID_EMPRESA || ') ';
    END IF;

    IF I_LISTA_ID_ADMINISTRADORA IS NOT NULL
    THEN
      V_SQL := V_SQL || ' AND M.ID_ADMINISTRADORA IN (' ||
               I_LISTA_ID_ADMINISTRADORA || ') ';
    END IF;

    IF I_LISTA_ID_TIPO_EMPRESA IS NOT NULL
    THEN
      V_SQL := V_SQL || ' AND M.ID_TIPO_EMPRESA in( ' ||
               I_LISTA_ID_TIPO_EMPRESA || ' )';
    END IF;

    IF I_LISTA_ID_TIPO_SOLICITANTE IS NOT NULL
    THEN
      V_SQL := V_SQL ||
               ' AND M.Id_Autorizacion in ( select id_autorizacion
                                 from MAASP_TUNI_TPTE.T_AUTORIZACIONES a
                                where a.id_autorizacion = m.Id_Autorizacion
                                  and a.tipo_solicitante IN (' ||
               I_LISTA_ID_TIPO_SOLICITANTE || ' ) ) ';
    END IF;

    IF I_LISTA_ID_VALIDACIONES IS NOT NULL
    THEN
      V_SQL := V_SQL ||
               'AND M.ID_MOVIMIENTO in( SELECT id_movimiento
                                         FROM T_MOVIMIENTO_RECHAZO
                                        WHERE id_movimiento = m.id_movimiento
                                          AND id_validacion IN(' ||
               I_LISTA_ID_VALIDACIONES || ' ) ) ';
    END IF;

    OPEN V_CUR FOR V_SQL
      USING I_FEC_MOVIMIENTO_DESDE, I_FEC_MOVIMIENTO_HASTA;
    LOOP

      FETCH V_CUR
        INTO V_ID_MOVIMIENTO;

      EXIT WHEN V_CUR%NOTFOUND;

      -- actualizo el estado
      UPDATE T_MOVIMIENTOS
         SET ID_RESULTADO = 4
       WHERE ID_MOVIMIENTO = V_ID_MOVIMIENTO;

      -- vinculo a la operacion
      INSERT INTO T_OPERACIONES_MOVIMIENTO
        (ID_OPERACION
        ,ID_MOVIMIENTO)
      VALUES
        (V_ID_OPERACION
        ,V_ID_MOVIMIENTO);

      COMMIT;

    END LOOP;

  END SP_APROBAR_BOLETOS;

END PKG_VALIDACIONES;
/