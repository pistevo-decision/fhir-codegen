// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// An error, warning, or information message that results from a system action.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<OperationOutcomeIssue>))]
  public class OperationOutcomeIssue : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Describes the type of the issue. The system that creates an OperationOutcome SHALL choose the most applicable code from the IssueType value set, and may additional provide its own code for the error in the details element.
    /// </summary>
    public string Code { get; set; }
    /// <summary>
    /// Extension container element for Code
    /// </summary>
    public Element _Code { get; set; }
    /// <summary>
    /// A human readable description of the error issue SHOULD be placed in details.text.
    /// </summary>
    public CodeableConcept Details { get; set; }
    /// <summary>
    /// This may be a description of how a value is erroneous, a stack dump to help trace the issue or other troubleshooting information.
    /// </summary>
    public string Diagnostics { get; set; }
    /// <summary>
    /// Extension container element for Diagnostics
    /// </summary>
    public Element _Diagnostics { get; set; }
    /// <summary>
    /// The root of the FHIRPath is the resource or bundle that generated OperationOutcome.  Each FHIRPath SHALL resolve to a single node.
    /// </summary>
    public List<string> Expression { get; set; }
    /// <summary>
    /// Extension container element for Expression
    /// </summary>
    public List<Element> _Expression { get; set; }
    /// <summary>
    /// The root of the XPath is the resource or bundle that generated OperationOutcome.  Each XPath SHALL resolve to a single node.  This element is deprecated, and is being replaced by expression.
    /// </summary>
    public List<string> Location { get; set; }
    /// <summary>
    /// Extension container element for Location
    /// </summary>
    public List<Element> _Location { get; set; }
    /// <summary>
    /// This is labeled as "Is Modifier" because applications should not confuse hints and warnings with errors.
    /// </summary>
    public string Severity { get; set; }
    /// <summary>
    /// Extension container element for Severity
    /// </summary>
    public Element _Severity { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Severity))
      {
        writer.WriteString("severity", (string)Severity!);
      }

      if (_Severity != null)
      {
        writer.WritePropertyName("_severity");
        _Severity.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Code))
      {
        writer.WriteString("code", (string)Code!);
      }

      if (_Code != null)
      {
        writer.WritePropertyName("_code");
        _Code.SerializeJson(writer, options);
      }

      if (Details != null)
      {
        writer.WritePropertyName("details");
        Details.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Diagnostics))
      {
        writer.WriteString("diagnostics", (string)Diagnostics!);
      }

      if (_Diagnostics != null)
      {
        writer.WritePropertyName("_diagnostics");
        _Diagnostics.SerializeJson(writer, options);
      }

      if ((Location != null) && (Location.Count != 0))
      {
        writer.WritePropertyName("location");
        writer.WriteStartArray();

        foreach (string valLocation in Location)
        {
          writer.WriteStringValue(valLocation);
        }

        writer.WriteEndArray();
      }

      if ((_Location != null) && (_Location.Count != 0))
      {
        writer.WritePropertyName("_location");
        writer.WriteStartArray();

        foreach (Element val_Location in _Location)
        {
          val_Location.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Expression != null) && (Expression.Count != 0))
      {
        writer.WritePropertyName("expression");
        writer.WriteStartArray();

        foreach (string valExpression in Expression)
        {
          writer.WriteStringValue(valExpression);
        }

        writer.WriteEndArray();
      }

      if ((_Expression != null) && (_Expression.Count != 0))
      {
        writer.WritePropertyName("_expression");
        writer.WriteStartArray();

        foreach (Element val_Expression in _Expression)
        {
          val_Expression.SerializeJson(writer, options, true);
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
          Code = reader.GetString();
          break;

        case "_code":
          _Code = new fhirCsR5.Models.Element();
          _Code.DeserializeJson(ref reader, options);
          break;

        case "details":
          Details = new fhirCsR5.Models.CodeableConcept();
          Details.DeserializeJson(ref reader, options);
          break;

        case "diagnostics":
          Diagnostics = reader.GetString();
          break;

        case "_diagnostics":
          _Diagnostics = new fhirCsR5.Models.Element();
          _Diagnostics.DeserializeJson(ref reader, options);
          break;

        case "expression":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Expression = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Expression.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Expression.Count == 0)
          {
            Expression = null;
          }

          break;

        case "_expression":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Expression = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Element obj_Expression = new fhirCsR5.Models.Element();
            obj_Expression.DeserializeJson(ref reader, options);
            _Expression.Add(obj_Expression);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Expression.Count == 0)
          {
            _Expression = null;
          }

          break;

        case "location":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Location = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Location.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Location.Count == 0)
          {
            Location = null;
          }

          break;

        case "_location":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Location = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Element obj_Location = new fhirCsR5.Models.Element();
            obj_Location.DeserializeJson(ref reader, options);
            _Location.Add(obj_Location);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Location.Count == 0)
          {
            _Location = null;
          }

          break;

        case "severity":
          Severity = reader.GetString();
          break;

        case "_severity":
          _Severity = new fhirCsR5.Models.Element();
          _Severity.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the OperationOutcome.issue.severity field
  /// </summary>
  public static class OperationOutcomeIssueSeverityCodes {
    public const string FATAL = "fatal";
    public const string ERROR = "error";
    public const string WARNING = "warning";
    public const string INFORMATION = "information";
  }
  /// <summary>
  /// A collection of error, warning, or information messages that result from a system action.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<OperationOutcome>))]
  public class OperationOutcome : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "OperationOutcome";
    /// <summary>
    /// An error, warning, or information message that results from a system action.
    /// </summary>
    public List<OperationOutcomeIssue> Issue { get; set; }
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


      ((fhirCsR5.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Issue != null) && (Issue.Count != 0))
      {
        writer.WritePropertyName("issue");
        writer.WriteStartArray();

        foreach (OperationOutcomeIssue valIssue in Issue)
        {
          valIssue.SerializeJson(writer, options, true);
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
        case "issue":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Issue = new List<OperationOutcomeIssue>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.OperationOutcomeIssue objIssue = new fhirCsR5.Models.OperationOutcomeIssue();
            objIssue.DeserializeJson(ref reader, options);
            Issue.Add(objIssue);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Issue.Count == 0)
          {
            Issue = null;
          }

          break;

        default:
          ((fhirCsR5.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
}
