// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the validity of a dose relative to a particular recommended schedule. This value set is provided as a suggestive example.
  /// </summary>
  public static class VaccinationProtocolDoseStatusCodes
  {
    /// <summary>
    /// The dose counts toward fulfilling a path to immunity for a patient, providing protection against the target disease.
    /// </summary>
    public static readonly Coding Counts = new Coding
    {
      Code = "count",
      Display = "Counts",
      System = "http://hl7.org/fhir/vaccination-protocol-dose-status"
    };
    /// <summary>
    /// The dose does not count toward fulfilling a path to immunity for a patient.
    /// </summary>
    public static readonly Coding DoesNotCount = new Coding
    {
      Code = "nocount",
      Display = "Does not Count",
      System = "http://hl7.org/fhir/vaccination-protocol-dose-status"
    };

    /// <summary>
    /// Literal for code: Counts
    /// </summary>
    public const string LiteralCounts = "count";

    /// <summary>
    /// Literal for code: DoesNotCount
    /// </summary>
    public const string LiteralDoesNotCount = "nocount";
  };
}
