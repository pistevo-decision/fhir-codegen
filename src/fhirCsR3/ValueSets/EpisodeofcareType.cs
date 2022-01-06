// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// This example value set defines a set of codes that can be used to express the usage type of an EpisodeOfCare record.
  /// </summary>
  public static class EpisodeofcareTypeCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CommunityBasedAgedCare = new Coding
    {
      Code = "cacp",
      Display = "Community-based aged care",
      System = "http://hl7.org/fhir/episodeofcare-type"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DrugAndAlcoholRehabilitation = new Coding
    {
      Code = "da",
      Display = "Drug and alcohol rehabilitation",
      System = "http://hl7.org/fhir/episodeofcare-type"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PostCoOrdinatedDiabetesProgram = new Coding
    {
      Code = "diab",
      Display = "Post co-ordinated diabetes program",
      System = "http://hl7.org/fhir/episodeofcare-type"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding HomeAndCommunityCare = new Coding
    {
      Code = "hacc",
      Display = "Home and Community Care",
      System = "http://hl7.org/fhir/episodeofcare-type"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PostAcuteCare = new Coding
    {
      Code = "pac",
      Display = "Post Acute Care",
      System = "http://hl7.org/fhir/episodeofcare-type"
    };

    /// <summary>
    /// Literal for code: CommunityBasedAgedCare
    /// </summary>
    public const string LiteralCommunityBasedAgedCare = "cacp";

    /// <summary>
    /// Literal for code: DrugAndAlcoholRehabilitation
    /// </summary>
    public const string LiteralDrugAndAlcoholRehabilitation = "da";

    /// <summary>
    /// Literal for code: PostCoOrdinatedDiabetesProgram
    /// </summary>
    public const string LiteralPostCoOrdinatedDiabetesProgram = "diab";

    /// <summary>
    /// Literal for code: HomeAndCommunityCare
    /// </summary>
    public const string LiteralHomeAndCommunityCare = "hacc";

    /// <summary>
    /// Literal for code: PostAcuteCare
    /// </summary>
    public const string LiteralPostAcuteCare = "pac";
  };
}
