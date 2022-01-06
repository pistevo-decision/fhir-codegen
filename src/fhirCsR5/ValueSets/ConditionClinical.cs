// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Preferred value set for Condition Clinical Status.
  /// </summary>
  public static class ConditionClinicalCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://terminology.hl7.org/CodeSystem/condition-clinical"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Inactive = new Coding
    {
      Code = "inactive",
      Display = "Inactive",
      System = "http://terminology.hl7.org/CodeSystem/condition-clinical"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Recurrence = new Coding
    {
      Code = "recurrence",
      Display = "Recurrence",
      System = "http://terminology.hl7.org/CodeSystem/condition-clinical"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Relapse = new Coding
    {
      Code = "relapse",
      Display = "Relapse",
      System = "http://terminology.hl7.org/CodeSystem/condition-clinical"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Remission = new Coding
    {
      Code = "remission",
      Display = "Remission",
      System = "http://terminology.hl7.org/CodeSystem/condition-clinical"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Resolved = new Coding
    {
      Code = "resolved",
      Display = "Resolved",
      System = "http://terminology.hl7.org/CodeSystem/condition-clinical"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: Inactive
    /// </summary>
    public const string LiteralInactive = "inactive";

    /// <summary>
    /// Literal for code: Recurrence
    /// </summary>
    public const string LiteralRecurrence = "recurrence";

    /// <summary>
    /// Literal for code: Relapse
    /// </summary>
    public const string LiteralRelapse = "relapse";

    /// <summary>
    /// Literal for code: Remission
    /// </summary>
    public const string LiteralRemission = "remission";

    /// <summary>
    /// Literal for code: Resolved
    /// </summary>
    public const string LiteralResolved = "resolved";
  };
}
