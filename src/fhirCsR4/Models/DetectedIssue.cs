// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4.Serialization;

namespace fhirCsR4.Models
{
  /// <summary>
  /// Supporting evidence or manifestations that provide the basis for identifying the detected issue such as a GuidanceResponse or MeasureReport.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<DetectedIssueEvidence>))]
  public class DetectedIssueEvidence : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// A manifestation that led to the recording of this detected issue.
    /// </summary>
    public List<CodeableConcept> Code { get; set; }
    /// <summary>
    /// Links to resources that constitute evidence for the detected issue such as a GuidanceResponse or MeasureReport.
    /// </summary>
    public List<Reference> Detail { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if ((Code != null) && (Code.Count != 0))
      {
        writer.WritePropertyName("code");
        writer.WriteStartArray();

        foreach (CodeableConcept valCode in Code)
        {
          valCode.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Detail != null) && (Detail.Count != 0))
      {
        writer.WritePropertyName("detail");
        writer.WriteStartArray();

        foreach (Reference valDetail in Detail)
        {
          valDetail.SerializeJson(writer, options, true);
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
        case "code":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Code = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CodeableConcept objCode = new fhirCsR4.Models.CodeableConcept();
            objCode.DeserializeJson(ref reader, options);
            Code.Add(objCode);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Code.Count == 0)
          {
            Code = null;
          }

          break;

        case "detail":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Detail = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Reference objDetail = new fhirCsR4.Models.Reference();
            objDetail.DeserializeJson(ref reader, options);
            Detail.Add(objDetail);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Detail.Count == 0)
          {
            Detail = null;
          }

          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Indicates an action that has been taken or is committed to reduce or eliminate the likelihood of the risk identified by the detected issue from manifesting.  Can also reflect an observation of known mitigating factors that may reduce/eliminate the need for any action.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<DetectedIssueMitigation>))]
  public class DetectedIssueMitigation : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The "text" component can be used for detail or when no appropriate code exists.
    /// </summary>
    public CodeableConcept Action { get; set; }
    /// <summary>
    /// Identifies the practitioner who determined the mitigation and takes responsibility for the mitigation step occurring.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// This might not be the same as when the mitigating step was actually taken.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Action != null)
      {
        writer.WritePropertyName("action");
        Action.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
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
        case "action":
          Action = new fhirCsR4.Models.CodeableConcept();
          Action.DeserializeJson(ref reader, options);
          break;

        case "author":
          Author = new fhirCsR4.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR4.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient; e.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<DetectedIssue>))]
  public class DetectedIssue : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "DetectedIssue";
    /// <summary>
    /// Individual or device responsible for the issue being raised.  For example, a decision support application or a pharmacist conducting a medication review.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// Identifies the general type of issue identified.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// Should focus on information not covered elsewhere as discrete data - no need to duplicate the narrative.
    /// </summary>
    public string Detail { get; set; }
    /// <summary>
    /// Extension container element for Detail
    /// </summary>
    public Element _Detail { get; set; }
    /// <summary>
    /// Supporting evidence or manifestations that provide the basis for identifying the detected issue such as a GuidanceResponse or MeasureReport.
    /// </summary>
    public List<DetectedIssueEvidence> Evidence { get; set; }
    /// <summary>
    /// The date or period when the detected issue was initially identified.
    /// </summary>
    public string IdentifiedDateTime { get; set; }
    /// <summary>
    /// Extension container element for IdentifiedDateTime
    /// </summary>
    public Element _IdentifiedDateTime { get; set; }
    /// <summary>
    /// The date or period when the detected issue was initially identified.
    /// </summary>
    public Period IdentifiedPeriod { get; set; }
    /// <summary>
    /// Business identifier associated with the detected issue record.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// There's an implicit constraint on the number of implicated resources based on DetectedIssue.type; e.g. For drug-drug, there would be more than one.  For timing, there would typically only be one.
    /// </summary>
    public List<Reference> Implicated { get; set; }
    /// <summary>
    /// Indicates an action that has been taken or is committed to reduce or eliminate the likelihood of the risk identified by the detected issue from manifesting.  Can also reflect an observation of known mitigating factors that may reduce/eliminate the need for any action.
    /// </summary>
    public List<DetectedIssueMitigation> Mitigation { get; set; }
    /// <summary>
    /// Indicates the patient whose record the detected issue is associated with.
    /// </summary>
    public Reference Patient { get; set; }
    /// <summary>
    /// The literature, knowledge-base or similar reference that describes the propensity for the detected issue identified.
    /// </summary>
    public string Reference { get; set; }
    /// <summary>
    /// Extension container element for Reference
    /// </summary>
    public Element _Reference { get; set; }
    /// <summary>
    /// Indicates the degree of importance associated with the identified issue based on the potential impact on the patient.
    /// </summary>
    public string Severity { get; set; }
    /// <summary>
    /// Extension container element for Severity
    /// </summary>
    public Element _Severity { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains the codes cancelled and entered-in-error that mark the issue as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
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


      ((fhirCsR4.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
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

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(IdentifiedDateTime))
      {
        writer.WriteString("identifiedDateTime", (string)IdentifiedDateTime!);
      }

      if (_IdentifiedDateTime != null)
      {
        writer.WritePropertyName("_identifiedDateTime");
        _IdentifiedDateTime.SerializeJson(writer, options);
      }

      if (IdentifiedPeriod != null)
      {
        writer.WritePropertyName("identifiedPeriod");
        IdentifiedPeriod.SerializeJson(writer, options);
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
      }

      if ((Implicated != null) && (Implicated.Count != 0))
      {
        writer.WritePropertyName("implicated");
        writer.WriteStartArray();

        foreach (Reference valImplicated in Implicated)
        {
          valImplicated.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Evidence != null) && (Evidence.Count != 0))
      {
        writer.WritePropertyName("evidence");
        writer.WriteStartArray();

        foreach (DetectedIssueEvidence valEvidence in Evidence)
        {
          valEvidence.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Detail))
      {
        writer.WriteString("detail", (string)Detail!);
      }

      if (_Detail != null)
      {
        writer.WritePropertyName("_detail");
        _Detail.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Reference))
      {
        writer.WriteString("reference", (string)Reference!);
      }

      if (_Reference != null)
      {
        writer.WritePropertyName("_reference");
        _Reference.SerializeJson(writer, options);
      }

      if ((Mitigation != null) && (Mitigation.Count != 0))
      {
        writer.WritePropertyName("mitigation");
        writer.WriteStartArray();

        foreach (DetectedIssueMitigation valMitigation in Mitigation)
        {
          valMitigation.SerializeJson(writer, options, true);
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
        case "author":
          Author = new fhirCsR4.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "code":
          Code = new fhirCsR4.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "detail":
          Detail = reader.GetString();
          break;

        case "_detail":
          _Detail = new fhirCsR4.Models.Element();
          _Detail.DeserializeJson(ref reader, options);
          break;

        case "evidence":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Evidence = new List<DetectedIssueEvidence>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.DetectedIssueEvidence objEvidence = new fhirCsR4.Models.DetectedIssueEvidence();
            objEvidence.DeserializeJson(ref reader, options);
            Evidence.Add(objEvidence);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Evidence.Count == 0)
          {
            Evidence = null;
          }

          break;

        case "identifiedDateTime":
          IdentifiedDateTime = reader.GetString();
          break;

        case "_identifiedDateTime":
          _IdentifiedDateTime = new fhirCsR4.Models.Element();
          _IdentifiedDateTime.DeserializeJson(ref reader, options);
          break;

        case "identifiedPeriod":
          IdentifiedPeriod = new fhirCsR4.Models.Period();
          IdentifiedPeriod.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Identifier objIdentifier = new fhirCsR4.Models.Identifier();
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

        case "implicated":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Implicated = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Reference objImplicated = new fhirCsR4.Models.Reference();
            objImplicated.DeserializeJson(ref reader, options);
            Implicated.Add(objImplicated);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Implicated.Count == 0)
          {
            Implicated = null;
          }

          break;

        case "mitigation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Mitigation = new List<DetectedIssueMitigation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.DetectedIssueMitigation objMitigation = new fhirCsR4.Models.DetectedIssueMitigation();
            objMitigation.DeserializeJson(ref reader, options);
            Mitigation.Add(objMitigation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Mitigation.Count == 0)
          {
            Mitigation = null;
          }

          break;

        case "patient":
          Patient = new fhirCsR4.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        case "reference":
          Reference = reader.GetString();
          break;

        case "_reference":
          _Reference = new fhirCsR4.Models.Element();
          _Reference.DeserializeJson(ref reader, options);
          break;

        case "severity":
          Severity = reader.GetString();
          break;

        case "_severity":
          _Severity = new fhirCsR4.Models.Element();
          _Severity.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR4.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the DetectedIssue.severity field
  /// </summary>
  public static class DetectedIssueSeverityCodes {
    public const string HIGH = "high";
    public const string MODERATE = "moderate";
    public const string LOW = "low";
  }
  /// <summary>
  /// Code Values for the DetectedIssue.status field
  /// </summary>
  public static class DetectedIssueStatusCodes {
    public const string REGISTERED = "registered";
    public const string PRELIMINARY = "preliminary";
    public const string FINAL = "final";
    public const string AMENDED = "amended";
    public const string CORRECTED = "corrected";
    public const string CANCELLED = "cancelled";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string UNKNOWN = "unknown";
  }
}
