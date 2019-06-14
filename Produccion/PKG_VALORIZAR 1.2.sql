CREATE OR REPLACE PACKAGE "PKG_VALORIZAR" IS

  -- calcula el valor de un boleto
  FUNCTION FU_VALOR_BOLETO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
    RETURN NUMBER;

  -- valorizar en lote
  PROCEDURE SP_VALORIZAR_BEG;
  PROCEDURE SP_VALORIZAR_BOS;

  -- limpieza de valorizacion ( para revalorizar ) 
  PROCEDURE SP_LIMPIAR_VALORIZACION(I_FECHA_DESDE               IN DATE,
                                    I_FECHA_HASTA               IN DATE,
                                    I_USUARIO                   IN VARCHAR2,
                                    I_OBSERVACIONES             IN VARCHAR2,
                                    I_LISTA_ID_EMPRESA          IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_ORIGEN           IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_DESTINO          IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_ADMINISTRADORA   IN VARCHAR2 DEFAULT NULL,
                                    I_CUIL                      IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_TIPO_EMPRESA     IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_TIPO_SOLICITANTE IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_RESULTADO        IN VARCHAR2 DEFAULT NULL,
                                    I_PROGRAMA                  IN T_MOVIMIENTOS.ID_PROGRAMA%TYPE,
                                    I_LISTA_ID_VALIDACIONES     IN VARCHAR2 DEFAULT NULL);

END PKG_VALORIZAR;
/
CREATE OR REPLACE PACKAGE BODY "PKG_VALORIZAR" IS

  /********************************************************************
  * Valorizacion por boleto                                           *
  *                                                                   *
  * Devuelve la valorizacion de un boleto acorde sus caracteristicas  *
  * En caso de no poder calcular devuelve nulo                        *
  * APTO PARA USO EN SQL                                              * 
  ********************************************************************/
  FUNCTION FU_VALOR_BOLETO(I_ID_MOVIMIENTO IN T_MOVIMIENTOS.ID_MOVIMIENTO%TYPE)
  
    RETURN NUMBER IS
  
    V_FEC_MOVIMIENTO        T_MOVIMIENTOS.FEC_MOVIMIENTO%TYPE;
    V_ID_EMPRESA            TARJUNI_TRANSP.T_AUTORIZACIONES.ID_EMPRESA%TYPE;
    V_ID_TIPO_EMPRESA       TARJUNI_TRANSP.T_AUTORIZACIONES.ID_TIPO_EMPRESA%TYPE;
    V_ID_ORIGEN             TARJUNI_TRANSP.T_AUTORIZACIONES.ID_ORIGEN%TYPE;
    V_ID_DESTINO            TARJUNI_TRANSP.T_AUTORIZACIONES.ID_DESTINO%TYPE;
    V_TIPO_SOLICITANTE      TARJUNI_TRANSP.T_AUTORIZACIONES.TIPO_SOLICITANTE%TYPE;
    V_ID_TIPO_BOLETO_URBANO T_TARIFAS_MUNICIPAL.ID_TIPO_BOLETO_URBANO%TYPE;
    V_PRECIO                NUMBER;
    V_COEFICIENTE           NUMBER;
  
  BEGIN
  
    -- busco los datos necesarios
    SELECT M.FEC_MOVIMIENTO,
           A.ID_EMPRESA,
           I.ID_TIPO_EMPRESA,
           A.ID_ORIGEN,
           A.ID_DESTINO,
           A.ID_TIPO_SOLICITANTE,
           M.TIPO_LINEA -- ## VERIFICAR SI EL TIPO DE BOLETO ESTA EN ESTE CAMPO ##
      INTO V_FEC_MOVIMIENTO,
           V_ID_EMPRESA,
           V_ID_TIPO_EMPRESA,
           V_ID_ORIGEN,
           V_ID_DESTINO,
           V_TIPO_SOLICITANTE,
           V_ID_TIPO_BOLETO_URBANO
      FROM MAASP_TUNI_TPTE.T_MOVIMIENTOS    M,
           MAASP_TUNI_TPTE.T_AUTORIZACIONES A,
           TRANSPORTE.T_EMPRESAS_X_TIPO     I
     WHERE M.ID_MOVIMIENTO = I_ID_MOVIMIENTO
       AND A.ID_AUTORIZACION = M.ID_AUTORIZACION
       AND M.ID_TIPO_EMPRESA = I.ID_TIPO_EMPRESA
       AND A.ID_EMPRESA = I.ID_EMPRESA; 
    
    -- para urbano interior
    IF V_ID_TIPO_EMPRESA = 1 THEN
    
      SELECT TM.PRECIO
        INTO V_PRECIO
        FROM T_TARIFAS_MUNICIPAL TM
       WHERE TM.ID_EMPRESA = V_ID_EMPRESA
         AND TM.ID_TIPO_SOLICITANTE = V_TIPO_SOLICITANTE
         AND NVL(TM.ID_TIPO_BOLETO_URBANO,-1) = NVL(V_ID_TIPO_BOLETO_URBANO,-1)
         AND TRUNC(V_FEC_MOVIMIENTO) BETWEEN TM.FEC_DESDE AND TM.FEC_HASTA;
    
      RETURN V_PRECIO;
    
      -- para interurbano/diferencial
    ELSIF V_ID_TIPO_EMPRESA IN (2, 3) THEN
    
      -- busco el precio por tramo y fecha
      SELECT P.IMPORTE
        INTO V_PRECIO
        FROM T_PRECIOS_TRAMO P
       WHERE P.ID_EMPRESA = V_ID_EMPRESA
         AND P.ID_TIPO_EMPRESA = V_ID_TIPO_EMPRESA
         AND P.ID_ORIGEN = V_ID_ORIGEN
         AND P.ID_DESTINO = V_ID_DESTINO
         AND TRUNC(V_FEC_MOVIMIENTO) BETWEEN P.FEC_DESDE AND
             NVL(P.FEC_HASTA, TRUNC(V_FEC_MOVIMIENTO))
         AND  P.FEC_HASTA IS NULL;
    
      -- busco el coeficiente por fecha
      BEGIN

        -- BUSCO PRIMERO POR EMPRESA
        SELECT C.COEFICIENTE
          INTO V_COEFICIENTE
          FROM T_COEFICIENTES_SOLICITANTE C
         WHERE C.ID_TIPO_SOLICITANTE = V_TIPO_SOLICITANTE
           AND C.ID_EMPRESA = V_ID_EMPRESA
           AND TRUNC(V_FEC_MOVIMIENTO) BETWEEN C.FEC_DESDE AND
               NVL(C.FEC_HASTA, TRUNC(V_FEC_MOVIMIENTO));
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          -- BUSCO EL GENERICO
          SELECT C.COEFICIENTE
            INTO V_COEFICIENTE
            FROM T_COEFICIENTES_SOLICITANTE C
           WHERE C.ID_TIPO_SOLICITANTE = V_TIPO_SOLICITANTE
             AND TRUNC(V_FEC_MOVIMIENTO) BETWEEN C.FEC_DESDE AND
                 NVL(C.FEC_HASTA, TRUNC(V_FEC_MOVIMIENTO));
      END;
    
      RETURN V_PRECIO * V_COEFICIENTE;
    
    END IF;
  
    -- si no cae en ningun pararmetro no tengo precio
    RETURN NULL;
  
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      -- si no existe alguno de los datos no tengo precio
      RETURN NULL;
  END FU_VALOR_BOLETO;

  /*************************************************************************************************
  * VALORIZAR EN LOTE                                                                              *
  *                                                                                                *
  * Toma los boletos definidos por los parametros que no esten valorizados y les calcula el precio *
  * los actualiza y los marca como valorizados. Siempre va a ignorar boletos ya liquidados         *
  *************************************************************************************************/
  PROCEDURE SP_VALORIZAR_BOS IS
  
    CURSOR C_LOTE IS
      SELECT M.ID_MOVIMIENTO, M.ID_TIPO_EMPRESA
        FROM T_MOVIMIENTOS                    M,
             MAASP_TUNI_TPTE.T_AUTORIZACIONES A,
             TRANSPORTE.T_EMPRESAS_X_TIPO     I
       WHERE A.ID_AUTORIZACION = M.ID_AUTORIZACION
         AND M.ID_TIPO_EMPRESA = I.ID_TIPO_EMPRESA
         AND A.ID_EMPRESA = I.ID_EMPRESA
         AND M.ID_TIPO_MOVIMIENTO = 1
         AND M.ID_ESTADO = 1
         AND M.ID_PROGRAMA = 2
         AND M.LIQUIDADO_SN IS NULL
         AND M.VALORIZAR_SN IS NULL;
  
    V_PRECIO NUMBER;
  
  BEGIN
  
    FOR L IN C_LOTE LOOP
    
      V_PRECIO := FU_VALOR_BOLETO(I_ID_MOVIMIENTO => L.ID_MOVIMIENTO);
    
      IF V_PRECIO IS NULL THEN
        -- no pudo calcular el precio
        UPDATE T_MOVIMIENTOS
           SET VALORIZAR_SN = 'N', FEC_VALORIZACION = SYSDATE
         WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
      
      ELSE
      
        IF L.ID_TIPO_EMPRESA = 1 THEN
          -- urbanos
          UPDATE T_MOVIMIENTOS
             SET VALORIZAR_SN     = 'S',
                 PRECIO_GOBIERNO  = V_PRECIO,
                 FEC_VALORIZACION = SYSDATE
           WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
        
        ELSIF L.ID_TIPO_EMPRESA IN (2, 3) THEN
        
          -- interurbanos/diferenciales
          UPDATE T_MOVIMIENTOS
             SET VALORIZAR_SN      = 'S',
                 PRECIO_VALORIZADO = V_PRECIO,
                 FEC_VALORIZACION  = SYSDATE
           WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
        
        ELSE
        
          -- tipo de empresa inexistente o no sportado
          UPDATE T_MOVIMIENTOS
             SET VALORIZAR_SN = 'N', FEC_VALORIZACION = SYSDATE
           WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
        
        END IF;
      
      END IF;
    
      COMMIT;
    
    END LOOP;
  
  END SP_VALORIZAR_BOS;
  
  PROCEDURE SP_VALORIZAR_BEG IS
  
    CURSOR C_LOTE IS
      SELECT M.ID_MOVIMIENTO, M.ID_TIPO_EMPRESA
        FROM T_MOVIMIENTOS                    M,
             MAASP_TUNI_TPTE.T_AUTORIZACIONES A,
             TRANSPORTE.T_EMPRESAS_X_TIPO     I
       WHERE A.ID_AUTORIZACION = M.ID_AUTORIZACION
         AND M.ID_TIPO_EMPRESA = I.ID_TIPO_EMPRESA
         AND A.ID_EMPRESA = I.ID_EMPRESA
         AND M.ID_TIPO_MOVIMIENTO = 1
         AND M.ID_ESTADO = 1
         AND M.ID_PROGRAMA = 1
         AND M.LIQUIDADO_SN IS NULL
         AND M.VALORIZAR_SN IS NULL;
  
    V_PRECIO NUMBER;
  
  BEGIN
  
    FOR L IN C_LOTE LOOP
    
      V_PRECIO := FU_VALOR_BOLETO(I_ID_MOVIMIENTO => L.ID_MOVIMIENTO);
    
      IF V_PRECIO IS NULL THEN
        -- no pudo calcular el precio
        UPDATE T_MOVIMIENTOS
           SET VALORIZAR_SN = 'N', FEC_VALORIZACION = SYSDATE
         WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
      
      ELSE
      
        IF L.ID_TIPO_EMPRESA = 1 THEN
          -- urbanos
          UPDATE T_MOVIMIENTOS
             SET VALORIZAR_SN     = 'S',
                 PRECIO_GOBIERNO  = V_PRECIO,
                 FEC_VALORIZACION = SYSDATE
           WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
        
        ELSIF L.ID_TIPO_EMPRESA IN (2, 3) THEN
        
          -- interurbanos/diferenciales
          UPDATE T_MOVIMIENTOS
             SET VALORIZAR_SN      = 'S',
                 PRECIO_VALORIZADO = V_PRECIO,
                 FEC_VALORIZACION  = SYSDATE
           WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
        
        ELSE
        
          -- tipo de empresa inexistente o no sportado
          UPDATE T_MOVIMIENTOS
             SET VALORIZAR_SN = 'N', FEC_VALORIZACION = SYSDATE
           WHERE ID_MOVIMIENTO = L.ID_MOVIMIENTO;
        
        END IF;
      
      END IF;
    
      COMMIT;
    
    END LOOP;
  
  END SP_VALORIZAR_BEG;

  /*************************************************************************************************
  * LIMPIAR VALORIZACION EN LOTE                                                                   *
  *                                                                                                *
  * Vuelve atras el proceso de valorizacion para el lote indicado, solo considera los boletos que  *
  * no han sido liquidados. El proceso automático de valorización va a retomar estos boletos en su *
  * corrida normal y revalorizarlos sin neceisdad de indicarle nada. Ignora los boletos que hayan  *
  * sido aprobados manualmente.                                                                    *
  *************************************************************************************************/
  PROCEDURE SP_LIMPIAR_VALORIZACION(I_FECHA_DESDE               IN DATE,
                                    I_FECHA_HASTA               IN DATE,
                                    I_USUARIO                   IN VARCHAR2,
                                    I_OBSERVACIONES             IN VARCHAR2,
                                    I_LISTA_ID_EMPRESA          IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_ORIGEN           IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_DESTINO          IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_ADMINISTRADORA   IN VARCHAR2 DEFAULT NULL,
                                    I_CUIL                      IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_TIPO_EMPRESA     IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_TIPO_SOLICITANTE IN VARCHAR2 DEFAULT NULL,
                                    I_LISTA_ID_RESULTADO        IN VARCHAR2 DEFAULT NULL,
                                    I_PROGRAMA                  IN T_MOVIMIENTOS.ID_PROGRAMA%TYPE,
                                    I_LISTA_ID_VALIDACIONES     IN VARCHAR2 DEFAULT NULL) IS
  
    PRAGMA AUTONOMOUS_TRANSACTION;
  
    V_SQL          VARCHAR2(10000);
    V_FILTROS      VARCHAR2(10000);
    V_ID_OPERACION T_OPERACIONES.ID_OPERACION%TYPE;
  
  BEGIN
  
    -- control parametros
    IF I_FECHA_DESDE IS NULL OR I_FECHA_HASTA IS NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'Deben indicarse ambos parametros de vigencia para correr el proceso de validaciones');
    
    END IF;
  
    IF I_USUARIO IS NULL OR I_OBSERVACIONES IS NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'Deben indicarse tanto el usuario de aplicación como el campo observaciones para correr esta funcionalidad');
    
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_EMPRESA, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_EMPRESA IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_EMPRESA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_ORIGEN, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_ORIGEN IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_ORIGEN tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_DESTINO, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_DESTINO IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_DESTINO tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_ADMINISTRADORA, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_ADMINISTRADORA IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_ADMINISTRADORA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_TIPO_EMPRESA, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_TIPO_EMPRESA IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_TIPO_EMPRESA tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_TIPO_SOLICITANTE, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_TIPO_SOLICITANTE IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_TIPO_SOLICITANTE tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_RESULTADO, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_RESULTADO IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro i_LISTA_ID_RESULTADO tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    IF NOT REGEXP_LIKE(I_LISTA_ID_VALIDACIONES, '^(\d+(,\d+)*)?$') AND
       I_LISTA_ID_VALIDACIONES IS NOT NULL THEN
      RAISE_APPLICATION_ERROR(-20001,
                              'El parametro I_LISTA_ID_VALIDACIONES tiene un valor incompatible con una lista de enteros separados por comas');
    END IF;
  
    -- armo los filtros
    IF I_LISTA_ID_EMPRESA IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_EMPRESA IN (' ||
                   I_LISTA_ID_EMPRESA || ') ';
    END IF;
  
    IF I_LISTA_ID_ORIGEN IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_ORIGEN IN (' ||
                   I_LISTA_ID_ORIGEN || ') ';
    END IF;
  
    IF I_LISTA_ID_DESTINO IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_DESTINO IN (' ||
                   I_LISTA_ID_DESTINO || ') ';
    END IF;
  
    IF I_LISTA_ID_ADMINISTRADORA IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_ADMINISTRADORA IN (' ||
                   I_LISTA_ID_ADMINISTRADORA || ') ';
    END IF;
  
    IF I_CUIL IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.CUIL = ' || I_CUIL;
    END IF;
  
    IF I_LISTA_ID_TIPO_EMPRESA IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_TIPO_EMPRESA IN(' ||
                   I_LISTA_ID_TIPO_EMPRESA || ') ';
    END IF;
  
    IF I_LISTA_ID_TIPO_SOLICITANTE IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_TIPO_SOLICITANTE IN(' ||
                   I_LISTA_ID_TIPO_SOLICITANTE || ') ';
    END IF;
  
    IF I_LISTA_ID_RESULTADO IS NOT NULL THEN
      V_FILTROS := V_FILTROS || ' AND M.ID_RESULTADO IN(' ||
                   I_LISTA_ID_RESULTADO || ') ';
    END IF;
  
    IF I_LISTA_ID_VALIDACIONES IS NOT NULL THEN
      V_FILTROS := V_FILTROS ||
                   ' AND EXISTS ( SELECT 1 
                                     FROM T_MOVIMIENTO_RECHAZO R
                                    WHERE R.ID_MOVIMIENTO = M.ID_MOVIMIENTO
                                      AND R.ID_VALIDACION IN (' ||
                   I_LISTA_ID_VALIDACIONES || ')) ';
    END IF;
  
    -- genero la operacion
    V_ID_OPERACION := SEQ_OPERACIONES.NEXTVAL;
  
    INSERT INTO T_OPERACIONES
      (ID_OPERACION,
       ID_TIPO_OPERACION,
       USUARIO_OPERACION,
       FEC_OPERACION,
       OBSERVACIONES)
    VALUES
      (V_ID_OPERACION, 'RT', I_USUARIO, SYSDATE, I_OBSERVACIONES);
  
    V_SQL := 'INSERT INTO T_OPERACIONES_MOVIMIENTO
      (ID_OPERACION
      ,ID_MOVIMIENTO)
      SELECT ' || V_ID_OPERACION || '
            ,M.ID_MOVIMIENTO
        FROM T_MOVIMIENTOS M
       WHERE M.FEC_MOVIMIENTO BETWEEN trunc(:fd) AND trunc(:fh+1) -1/24/60/60
                      AND M.ID_TIPO_MOVIMIENTO = 1
                      AND M.ID_ESTADO = 1
                      AND NVL(M.ID_RESULTADO,0) != 4
                      AND M.LIQUIDADO_SN IS NULL' || V_FILTROS;
  
    EXECUTE IMMEDIATE V_SQL
      USING I_FECHA_DESDE, I_FECHA_HASTA;
  
    V_SQL := 'UPDATE T_MOVIMIENTOS M
                      SET M.VALORIZAR_SN  = NULL
                        , M.FEC_VALORIZACION = NULL
                        , M.PRECIO_GOBIERNO = NULL
                        , M.PRECIO_VALORIZADO = NULL
                    WHERE M.FEC_MOVIMIENTO BETWEEN trunc(:fd) AND trunc(:fh+1) -1/24/60/60
                      AND M.ID_TIPO_MOVIMIENTO = 1
                      AND M.ID_ESTADO = 1
                      AND NVL(M.ID_RESULTADO,0) != 4
                      AND M.ID_PROGRAMA = '|| I_PROGRAMA ||
                      ' AND M.LIQUIDADO_SN IS NULL ' || V_FILTROS;
  
    DBMS_OUTPUT.PUT_LINE(V_SQL);
  
    EXECUTE IMMEDIATE V_SQL
      USING I_FECHA_DESDE, I_FECHA_HASTA;
  
    -- si no encuentra boletos descarto los cambions y corto el proceso
    IF SQL%ROWCOUNT = 0 THEN
      ROLLBACK;
      RETURN;
    END IF;
  
    COMMIT;
  
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE_APPLICATION_ERROR(-20001,
                              'Error al limpiar la valorización de boletos - ' ||
                              SQLERRM);
  END SP_LIMPIAR_VALORIZACION;

END PKG_VALORIZAR;
/