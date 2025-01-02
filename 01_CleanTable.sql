-- 提取睡眠時間、維生素D資料，並直接生成最終結果
WITH unique_data AS (
    SELECT
        sleeptime.checkKey,
        headers.sex,
        
        -- 將 sex 欄位轉換為 0 和 1，並另存為 SSex 欄位
        CASE 
            WHEN headers.sex = 'F' THEN 0 
            WHEN headers.sex = 'M' THEN 1 
        END AS SSex,
        
        headers.age,
        NULLIF(regexp_replace(sleeptime.checkResult, '[^0-9.]', '', 'g'), '')::NUMERIC AS sleeptimeCheckResult,
        sleeptime.fieldName AS sleeptimeFieldName,
        vitmaind.fieldName AS vitmaindFieldName,
        vitmaind.checkResult::NUMERIC AS vitmaindCheckResult,  -- 將 vitmaind.checkResult 轉換為數值型別
        ROW_NUMBER() OVER (PARTITION BY sleeptime.checkKey ORDER BY sleeptime.checkKey) AS row_num
    FROM
        (SELECT
            checkKey,
            fieldName,
            checkResult
         FROM
            details
         WHERE
            fieldName LIKE '%睡眠時間%') AS sleeptime
    JOIN
        (SELECT
            checkKey,
            fieldName,
            checkResult
         FROM
            details
         WHERE
            fieldName LIKE '%維生素D%') AS vitmaind
    ON sleeptime.checkKey = vitmaind.checkKey
    JOIN
        headers
    ON sleeptime.checkKey = headers.checkKey
    WHERE
        NULLIF(regexp_replace(sleeptime.checkResult, '[^0-9.]', '', 'g'), '') IS NOT NULL
)

-- 最終結果：選擇唯一記錄，並新增睡眠和維生素D分類
SELECT
    u.checkKey,
    
    -- 保留原始的 sex 欄位
    u.sex,
    
    -- 新增轉換後的 SSex 欄位
    u.SSex AS Sex,
    
    u.age,
    u.sleeptimeCheckResult,
    
    -- 根據睡眠時長進行分組：less5, 6, 7, 8, over9
    CASE
        WHEN u.sleeptimeCheckResult <= 5 THEN 'less5'
        WHEN u.sleeptimeCheckResult = 6 THEN '6'
        WHEN u.sleeptimeCheckResult = 7 THEN '7'
        WHEN u.sleeptimeCheckResult = 8 THEN '8'
        WHEN u.sleeptimeCheckResult >= 9 THEN 'over9'
    END AS SleepGroup,
    
    u.vitmaindCheckResult,
    
    -- 根據維生素D值進行分組
    CASE
        WHEN u.vitmaindCheckResult < 20 THEN 'Deficiency'
        WHEN u.vitmaindCheckResult < 30 THEN 'Insufficiency'
        ELSE 'Normal'
    END AS vitD_3_status,
    
    -- 新增 vitd_2_status 欄位
    CASE
        WHEN u.vitmaindCheckResult < 30 THEN 'Abnormal'
        ELSE 'Normal'
    END AS vitd_2_status
FROM
    unique_data u
WHERE
    u.row_num = 1
ORDER BY
    u.checkKey;
