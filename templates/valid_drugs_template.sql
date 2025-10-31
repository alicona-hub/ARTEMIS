WITH selected_atc AS (
  SELECT concept_id_atc, concept_code_atc
  FROM public.stg_atc_rxnorm
  WHERE concept_code_atc IN (
    'L01FD01',  -- trastuzumab
    'L01FD02',  -- pertuzumab
  )
), rxnorm_ingredients AS (
  SELECT DISTINCT
    s.concept_id_atc,
    s.concept_code_atc,
    r.concept_id_rxnorm AS concept_id_rxnorm
  FROM selected_atc s
  JOIN public.stg_atc_rxnorm r
    ON s.concept_id_atc = r.concept_id_atc
), rxnorm_to_hemonc AS (
  SELECT DISTINCT
    r.concept_id_rxnorm,
    c_rx.concept_name AS rxnorm_name,
    cr.concept_id_2 AS concept_id_hemonc,
    c_hemonc.concept_name AS hemonc_name,
    c_hemonc.domain_id,
    c_hemonc.concept_class_id
  FROM rxnorm_ingredients r
  JOIN public.concept c_rx
    ON r.concept_id_rxnorm = c_rx.concept_id
  JOIN public.concept_relationship cr
    ON r.concept_id_rxnorm = cr.concept_id_1
  JOIN public.concept c_hemonc
    ON cr.concept_id_2 = c_hemonc.concept_id
  WHERE c_hemonc.vocabulary_id = 'HemOnc'
    AND c_hemonc.invalid_reason IS NULL
    AND c_hemonc.domain_id = 'Drug'
    AND c_hemonc.concept_class_id = 'Component'
)
SELECT DISTINCT
    COALESCE(h.hemonc_name, c_rx.concept_name) AS name,
    COALESCE(h.concept_id_hemonc, r.concept_id_rxnorm) AS concept_id,
    COALESCE(h.concept_id_hemonc, r.concept_id_rxnorm) AS Manual,
    c_rx.concept_name AS concept_me,
    r.concept_id_rxnorm AS valid_concept_id,
    COALESCE(h.domain_id, c_rx.domain_id) AS domain_id,
    COALESCE(h.concept_class_id, c_rx.concept_class_id) AS concept_class_id,
    NULL AS Manual_Req,
    r.concept_code_atc
FROM rxnorm_ingredients r
JOIN public.concept c_rx ON r.concept_id_rxnorm = c_rx.concept_id
LEFT JOIN rxnorm_to_hemonc h   ON r.concept_id_rxnorm = h.concept_id_rxnorm