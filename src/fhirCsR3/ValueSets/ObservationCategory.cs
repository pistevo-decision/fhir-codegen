// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Observation Category codes.
  /// </summary>
  public static class ObservationCategoryCodes
  {
    /// <summary>
    /// Observations generated by physical exam findings including direct observations made by a clinician and use of simple instruments and the result of simple maneuvers performed directly on the patient's body.
    /// </summary>
    public static readonly Coding Exam = new Coding
    {
      Code = "exam",
      Display = "Exam",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    /// Observations generated by imaging. The scope includes observations, plain x-ray, ultrasound, CT, MRI, angiography, echocardiography, nuclear medicine.
    /// </summary>
    public static readonly Coding Imaging = new Coding
    {
      Code = "imaging",
      Display = "Imaging",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    /// The results of observations generated by laboratories.  Laboratory results are typically generated by laboratories providing analytic services in areas such as chemistry, hematology, serology, histology, cytology, anatomic pathology, microbiology, and/or virology. These observations are based on analysis of specimens obtained from the patient and submitted to the laboratory.
    /// </summary>
    public static readonly Coding Laboratory = new Coding
    {
      Code = "laboratory",
      Display = "Laboratory",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    /// Observations generated by other procedures.  This category includes observations resulting from interventional and non-interventional procedures excluding lab and imaging (e.g. cardiology catheterization, endoscopy, electrodiagnostics, etc.).  Procedure results are typically generated by a clinician to provide more granular information about component observations made during a procedure, such as where a gastroenterologist reports the size of a polyp observed during a colonoscopy.
    /// </summary>
    public static readonly Coding Procedure = new Coding
    {
      Code = "procedure",
      Display = "Procedure",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    /// The Social History Observations define the patient's occupational, personal (e.g. lifestyle), social, and environmental history and health risk factors, as well as administrative data such as marital status, race, ethnicity and religious affiliation.
    /// </summary>
    public static readonly Coding SocialHistory = new Coding
    {
      Code = "social-history",
      Display = "Social History",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    /// Assessment tool/survey instrument observations (e.g. Apgar Scores, Montreal Cognitive Assessment (MoCA))
    /// </summary>
    public static readonly Coding Survey = new Coding
    {
      Code = "survey",
      Display = "Survey",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    /// Observations generated by non-interventional treatment protocols (e.g. occupational, physical, radiation, nutritional and medication therapy)
    /// </summary>
    public static readonly Coding Therapy = new Coding
    {
      Code = "therapy",
      Display = "Therapy",
      System = "http://hl7.org/fhir/observation-category"
    };
    /// <summary>
    ///  Clinical observations measure the body's basic functions such as such as blood pressure, heart rate, respiratory rate, height, weight, body mass index, head circumference, pulse oximetry, temperature, and body surface area.
    /// </summary>
    public static readonly Coding VitalSigns = new Coding
    {
      Code = "vital-signs",
      Display = "Vital Signs",
      System = "http://hl7.org/fhir/observation-category"
    };

    /// <summary>
    /// Literal for code: Exam
    /// </summary>
    public const string LiteralExam = "exam";

    /// <summary>
    /// Literal for code: Imaging
    /// </summary>
    public const string LiteralImaging = "imaging";

    /// <summary>
    /// Literal for code: Laboratory
    /// </summary>
    public const string LiteralLaboratory = "laboratory";

    /// <summary>
    /// Literal for code: Procedure
    /// </summary>
    public const string LiteralProcedure = "procedure";

    /// <summary>
    /// Literal for code: SocialHistory
    /// </summary>
    public const string LiteralSocialHistory = "social-history";

    /// <summary>
    /// Literal for code: Survey
    /// </summary>
    public const string LiteralSurvey = "survey";

    /// <summary>
    /// Literal for code: Therapy
    /// </summary>
    public const string LiteralTherapy = "therapy";

    /// <summary>
    /// Literal for code: VitalSigns
    /// </summary>
    public const string LiteralVitalSigns = "vital-signs";
  };
}
