// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The type of the measure report
  /// </summary>
  public static class MeasureReportTypeCodes
  {
    /// <summary>
    /// An individual report that provides information on the performance for a given measure with respect to a single patient
    /// </summary>
    public static readonly Coding Individual = new Coding
    {
      Code = "individual",
      Display = "Individual",
      System = "http://hl7.org/fhir/measure-report-type"
    };
    /// <summary>
    /// A patient list report that includes a listing of patients that satisfied each population criteria in the measure
    /// </summary>
    public static readonly Coding PatientList = new Coding
    {
      Code = "patient-list",
      Display = "Patient List",
      System = "http://hl7.org/fhir/measure-report-type"
    };
    /// <summary>
    /// A summary report that returns the number of patients in each population criteria for the measure
    /// </summary>
    public static readonly Coding Summary = new Coding
    {
      Code = "summary",
      Display = "Summary",
      System = "http://hl7.org/fhir/measure-report-type"
    };

    /// <summary>
    /// Literal for code: Individual
    /// </summary>
    public const string LiteralIndividual = "individual";

    /// <summary>
    /// Literal for code: PatientList
    /// </summary>
    public const string LiteralPatientList = "patient-list";

    /// <summary>
    /// Literal for code: Summary
    /// </summary>
    public const string LiteralSummary = "summary";
  };
}
