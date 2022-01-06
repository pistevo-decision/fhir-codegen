// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR2.Serialization;

namespace fhirCsR2.Models
{
  /// <summary>
  /// Details about each adverse reaction event linked to exposure to the identified Substance.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<AllergyIntoleranceReaction>))]
  public class AllergyIntoleranceReaction : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Statement about the degree of clinical certainty that the specific substance was the cause of the manifestation in this reaction event.
    /// </summary>
    public string Certainty { get; set; }
    /// <summary>
    /// Extension container element for Certainty
    /// </summary>
    public Element _Certainty { get; set; }
    /// <summary>
    /// Text description about the reaction as a whole, including details of the manifestation if required.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// Identification of the route by which the subject was exposed to the substance.
    /// </summary>
    public CodeableConcept ExposureRoute { get; set; }
    /// <summary>
    /// Clinical symptoms and/or signs that are observed or associated with the adverse reaction event.
    /// </summary>
    public List<CodeableConcept> Manifestation { get; set; }
    /// <summary>
    /// Additional text about the adverse reaction event not captured in other fields.
    /// </summary>
    public Annotation Note { get; set; }
    /// <summary>
    /// Record of the date and/or time of the onset of the Reaction.
    /// </summary>
    public string Onset { get; set; }
    /// <summary>
    /// Extension container element for Onset
    /// </summary>
    public Element _Onset { get; set; }
    /// <summary>
    /// Clinical assessment of the severity of the reaction event as a whole, potentially considering multiple different manifestations.
    /// </summary>
    public string Severity { get; set; }
    /// <summary>
    /// Extension container element for Severity
    /// </summary>
    public Element _Severity { get; set; }
    /// <summary>
    /// Identification of the specific substance considered to be responsible for the Adverse Reaction event. Note: the substance for a specific reaction may be different to the substance identified as the cause of the risk, but must be consistent with it. For instance, it may be a more specific substance (e.g. a brand medication) or a composite substance that includes the identified substance. It must be clinically safe to only process the AllergyIntolerance.substance and ignore the AllergyIntolerance.event.substance.
    /// </summary>
    public CodeableConcept Substance { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR2.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Substance != null)
      {
        writer.WritePropertyName("substance");
        Substance.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Certainty))
      {
        writer.WriteString("certainty", (string)Certainty!);
      }

      if (_Certainty != null)
      {
        writer.WritePropertyName("_certainty");
        _Certainty.SerializeJson(writer, options);
      }

      if ((Manifestation != null) && (Manifestation.Count != 0))
      {
        writer.WritePropertyName("manifestation");
        writer.WriteStartArray();

        foreach (CodeableConcept valManifestation in Manifestation)
        {
          valManifestation.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Onset))
      {
        writer.WriteString("onset", (string)Onset!);
      }

      if (_Onset != null)
      {
        writer.WritePropertyName("_onset");
        _Onset.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Severity))
      {
        writer.WriteString("severity", (string)Severity!);
      }

      if (_Severity != null)
      {
        writer.WritePropertyName("_severity");
        _Severity.SerializeJson(writer, options);
      }

      if (ExposureRoute != null)
      {
        writer.WritePropertyName("exposureRoute");
        ExposureRoute.SerializeJson(writer, options);
      }

      if (Note != null)
      {
        writer.WritePropertyName("note");
        Note.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "certainty":
          Certainty = reader.GetString();
          break;

        case "_certainty":
          _Certainty = new fhirCsR2.Models.Element();
          _Certainty.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR2.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "exposureRoute":
          ExposureRoute = new fhirCsR2.Models.CodeableConcept();
          ExposureRoute.DeserializeJson(ref reader, options);
          break;

        case "manifestation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Manifestation = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CodeableConcept objManifestation = new fhirCsR2.Models.CodeableConcept();
            objManifestation.DeserializeJson(ref reader, options);
            Manifestation.Add(objManifestation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Manifestation.Count == 0)
          {
            Manifestation = null;
          }

          break;

        case "note":
          Note = new fhirCsR2.Models.Annotation();
          Note.DeserializeJson(ref reader, options);
          break;

        case "onset":
          Onset = reader.GetString();
          break;

        case "_onset":
          _Onset = new fhirCsR2.Models.Element();
          _Onset.DeserializeJson(ref reader, options);
          break;

        case "severity":
          Severity = reader.GetString();
          break;

        case "_severity":
          _Severity = new fhirCsR2.Models.Element();
          _Severity.DeserializeJson(ref reader, options);
          break;

        case "substance":
          Substance = new fhirCsR2.Models.CodeableConcept();
          Substance.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the AllergyIntolerance.reaction.certainty field
  /// </summary>
  public static class AllergyIntoleranceReactionCertaintyCodes {
    public const string UNLIKELY = "unlikely";
    public const string LIKELY = "likely";
    public const string CONFIRMED = "confirmed";
  }
  /// <summary>
  /// Code Values for the AllergyIntolerance.reaction.severity field
  /// </summary>
  public static class AllergyIntoleranceReactionSeverityCodes {
    public const string MILD = "mild";
    public const string MODERATE = "moderate";
    public const string SEVERE = "severe";
  }
  /// <summary>
  /// Risk of harmful or undesirable, physiological response which is unique to an individual and associated with exposure to a substance.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<AllergyIntolerance>))]
  public class AllergyIntolerance : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "AllergyIntolerance";
    /// <summary>
    /// Category of the identified Substance.
    /// </summary>
    public string Category { get; set; }
    /// <summary>
    /// Extension container element for Category
    /// </summary>
    public Element _Category { get; set; }
    /// <summary>
    /// Estimate of the potential clinical harm, or seriousness, of the reaction to the identified Substance.
    /// </summary>
    public string Criticality { get; set; }
    /// <summary>
    /// Extension container element for Criticality
    /// </summary>
    public Element _Criticality { get; set; }
    /// <summary>
    /// This records identifiers associated with this allergy/intolerance concern that are defined by business processes and/or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Represents the date and/or time of the last known occurrence of a reaction event.
    /// </summary>
    public string LastOccurence { get; set; }
    /// <summary>
    /// Extension container element for LastOccurence
    /// </summary>
    public Element _LastOccurence { get; set; }
    /// <summary>
    /// Additional narrative about the propensity for the Adverse Reaction, not captured in other fields.
    /// </summary>
    public Annotation Note { get; set; }
    /// <summary>
    /// Record of the date and/or time of the onset of the Allergy or Intolerance.
    /// </summary>
    public string Onset { get; set; }
    /// <summary>
    /// Extension container element for Onset
    /// </summary>
    public Element _Onset { get; set; }
    /// <summary>
    /// The patient who has the allergy or intolerance.
    /// </summary>
    public Reference Patient { get; set; }
    /// <summary>
    /// Details about each adverse reaction event linked to exposure to the identified Substance.
    /// </summary>
    public List<AllergyIntoleranceReaction> Reaction { get; set; }
    /// <summary>
    /// Date when the sensitivity was recorded.
    /// </summary>
    public string RecordedDate { get; set; }
    /// <summary>
    /// Extension container element for RecordedDate
    /// </summary>
    public Element _RecordedDate { get; set; }
    /// <summary>
    /// Individual who recorded the record and takes responsibility for its conten.
    /// </summary>
    public Reference Recorder { get; set; }
    /// <summary>
    /// The source of the information about the allergy that is recorded.
    /// </summary>
    public Reference Reporter { get; set; }
    /// <summary>
    /// Assertion about certainty associated with the propensity, or potential risk, of a reaction to the identified Substance.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Identification of a substance, or a class of substances, that is considered to be responsible for the adverse reaction risk.
    /// </summary>
    public CodeableConcept Substance { get; set; }
    /// <summary>
    /// Identification of the underlying physiological mechanism for the reaction risk.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR2.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Onset))
      {
        writer.WriteString("onset", (string)Onset!);
      }

      if (_Onset != null)
      {
        writer.WritePropertyName("_onset");
        _Onset.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(RecordedDate))
      {
        writer.WriteString("recordedDate", (string)RecordedDate!);
      }

      if (_RecordedDate != null)
      {
        writer.WritePropertyName("_recordedDate");
        _RecordedDate.SerializeJson(writer, options);
      }

      if (Recorder != null)
      {
        writer.WritePropertyName("recorder");
        Recorder.SerializeJson(writer, options);
      }

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
      }

      if (Reporter != null)
      {
        writer.WritePropertyName("reporter");
        Reporter.SerializeJson(writer, options);
      }

      if (Substance != null)
      {
        writer.WritePropertyName("substance");
        Substance.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Criticality))
      {
        writer.WriteString("criticality", (string)Criticality!);
      }

      if (_Criticality != null)
      {
        writer.WritePropertyName("_criticality");
        _Criticality.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Category))
      {
        writer.WriteString("category", (string)Category!);
      }

      if (_Category != null)
      {
        writer.WritePropertyName("_category");
        _Category.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(LastOccurence))
      {
        writer.WriteString("lastOccurence", (string)LastOccurence!);
      }

      if (_LastOccurence != null)
      {
        writer.WritePropertyName("_lastOccurence");
        _LastOccurence.SerializeJson(writer, options);
      }

      if (Note != null)
      {
        writer.WritePropertyName("note");
        Note.SerializeJson(writer, options);
      }

      if ((Reaction != null) && (Reaction.Count != 0))
      {
        writer.WritePropertyName("reaction");
        writer.WriteStartArray();

        foreach (AllergyIntoleranceReaction valReaction in Reaction)
        {
          valReaction.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "category":
          Category = reader.GetString();
          break;

        case "_category":
          _Category = new fhirCsR2.Models.Element();
          _Category.DeserializeJson(ref reader, options);
          break;

        case "criticality":
          Criticality = reader.GetString();
          break;

        case "_criticality":
          _Criticality = new fhirCsR2.Models.Element();
          _Criticality.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Identifier objIdentifier = new fhirCsR2.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "lastOccurence":
          LastOccurence = reader.GetString();
          break;

        case "_lastOccurence":
          _LastOccurence = new fhirCsR2.Models.Element();
          _LastOccurence.DeserializeJson(ref reader, options);
          break;

        case "note":
          Note = new fhirCsR2.Models.Annotation();
          Note.DeserializeJson(ref reader, options);
          break;

        case "onset":
          Onset = reader.GetString();
          break;

        case "_onset":
          _Onset = new fhirCsR2.Models.Element();
          _Onset.DeserializeJson(ref reader, options);
          break;

        case "patient":
          Patient = new fhirCsR2.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        case "reaction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Reaction = new List<AllergyIntoleranceReaction>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.AllergyIntoleranceReaction objReaction = new fhirCsR2.Models.AllergyIntoleranceReaction();
            objReaction.DeserializeJson(ref reader, options);
            Reaction.Add(objReaction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Reaction.Count == 0)
          {
            Reaction = null;
          }

          break;

        case "recordedDate":
          RecordedDate = reader.GetString();
          break;

        case "_recordedDate":
          _RecordedDate = new fhirCsR2.Models.Element();
          _RecordedDate.DeserializeJson(ref reader, options);
          break;

        case "recorder":
          Recorder = new fhirCsR2.Models.Reference();
          Recorder.DeserializeJson(ref reader, options);
          break;

        case "reporter":
          Reporter = new fhirCsR2.Models.Reference();
          Reporter.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR2.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "substance":
          Substance = new fhirCsR2.Models.CodeableConcept();
          Substance.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR2.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the AllergyIntolerance.category field
  /// </summary>
  public static class AllergyIntoleranceCategoryCodes {
    public const string FOOD = "food";
    public const string MEDICATION = "medication";
    public const string ENVIRONMENT = "environment";
    public const string OTHER = "other";
  }
  /// <summary>
  /// Code Values for the AllergyIntolerance.criticality field
  /// </summary>
  public static class AllergyIntoleranceCriticalityCodes {
    public const string CRITL = "CRITL";
    public const string CRITH = "CRITH";
    public const string CRITU = "CRITU";
  }
  /// <summary>
  /// Code Values for the AllergyIntolerance.status field
  /// </summary>
  public static class AllergyIntoleranceStatusCodes {
    public const string ACTIVE = "active";
    public const string UNCONFIRMED = "unconfirmed";
    public const string CONFIRMED = "confirmed";
    public const string INACTIVE = "inactive";
    public const string RESOLVED = "resolved";
    public const string REFUTED = "refuted";
    public const string ENTERED_IN_ERROR = "entered-in-error";
  }
  /// <summary>
  /// Code Values for the AllergyIntolerance.type field
  /// </summary>
  public static class AllergyIntoleranceTypeCodes {
    public const string ALLERGY = "allergy";
    public const string INTOLERANCE = "intolerance";
  }
}
