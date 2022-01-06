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
  /// A component comment, classifier, or rating of the artifact.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<ArtifactAssessmentContent>))]
  public class ArtifactAssessmentContent : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Indicates who or what authored the content.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// Represents a rating, classifier, or assessment of the artifact.
    /// </summary>
    public List<CodeableConcept> Classifier { get; set; }
    /// <summary>
    /// If the informationType is container, the components of the content.
    /// </summary>
    public List<ArtifactAssessmentContent> Component { get; set; }
    /// <summary>
    /// Acceptable to publicly share the comment, classifier or rating.
    /// </summary>
    public bool? FreeToShare { get; set; }
    /// <summary>
    /// The type of information this component of the content represents.
    /// </summary>
    public string InformationType { get; set; }
    /// <summary>
    /// Extension container element for InformationType
    /// </summary>
    public Element _InformationType { get; set; }
    /// <summary>
    /// The target element is used to point the comment to aspect of the artifact, such as a text range within a CQL library (e.g. #content?0:0-120:80).
    /// </summary>
    public List<string> Path { get; set; }
    /// <summary>
    /// Extension container element for Path
    /// </summary>
    public List<Element> _Path { get; set; }
    /// <summary>
    /// Additional related artifacts that provide supporting documentation, additional evidence, or further information related to the content.
    /// </summary>
    public List<RelatedArtifact> RelatedArtifact { get; set; }
    /// <summary>
    /// A brief summary of the content of this component.
    /// </summary>
    public string Summary { get; set; }
    /// <summary>
    /// Extension container element for Summary
    /// </summary>
    public Element _Summary { get; set; }
    /// <summary>
    /// Indicates what type of content this component represents.
    /// </summary>
    public CodeableConcept Type { get; set; }
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

      if (!string.IsNullOrEmpty(InformationType))
      {
        writer.WriteString("informationType", (string)InformationType!);
      }

      if (_InformationType != null)
      {
        writer.WritePropertyName("_informationType");
        _InformationType.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Summary))
      {
        writer.WriteString("summary", (string)Summary!);
      }

      if (_Summary != null)
      {
        writer.WritePropertyName("_summary");
        _Summary.SerializeJson(writer, options);
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if ((Classifier != null) && (Classifier.Count != 0))
      {
        writer.WritePropertyName("classifier");
        writer.WriteStartArray();

        foreach (CodeableConcept valClassifier in Classifier)
        {
          valClassifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
      }

      if ((Path != null) && (Path.Count != 0))
      {
        writer.WritePropertyName("path");
        writer.WriteStartArray();

        foreach (string valPath in Path)
        {
          writer.WriteStringValue(valPath);
        }

        writer.WriteEndArray();
      }

      if ((_Path != null) && (_Path.Count != 0))
      {
        writer.WritePropertyName("_path");
        writer.WriteStartArray();

        foreach (Element val_Path in _Path)
        {
          val_Path.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((RelatedArtifact != null) && (RelatedArtifact.Count != 0))
      {
        writer.WritePropertyName("relatedArtifact");
        writer.WriteStartArray();

        foreach (RelatedArtifact valRelatedArtifact in RelatedArtifact)
        {
          valRelatedArtifact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (FreeToShare != null)
      {
        writer.WriteBoolean("freeToShare", (bool)FreeToShare!);
      }

      if ((Component != null) && (Component.Count != 0))
      {
        writer.WritePropertyName("component");
        writer.WriteStartArray();

        foreach (ArtifactAssessmentContent valComponent in Component)
        {
          valComponent.SerializeJson(writer, options, true);
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
          Author = new fhirCsR5.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "classifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Classifier = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objClassifier = new fhirCsR5.Models.CodeableConcept();
            objClassifier.DeserializeJson(ref reader, options);
            Classifier.Add(objClassifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Classifier.Count == 0)
          {
            Classifier = null;
          }

          break;

        case "component":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Component = new List<ArtifactAssessmentContent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.ArtifactAssessmentContent objComponent = new fhirCsR5.Models.ArtifactAssessmentContent();
            objComponent.DeserializeJson(ref reader, options);
            Component.Add(objComponent);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Component.Count == 0)
          {
            Component = null;
          }

          break;

        case "freeToShare":
          FreeToShare = reader.GetBoolean();
          break;

        case "informationType":
          InformationType = reader.GetString();
          break;

        case "_informationType":
          _InformationType = new fhirCsR5.Models.Element();
          _InformationType.DeserializeJson(ref reader, options);
          break;

        case "path":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Path = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Path.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Path.Count == 0)
          {
            Path = null;
          }

          break;

        case "_path":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Path = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Element obj_Path = new fhirCsR5.Models.Element();
            obj_Path.DeserializeJson(ref reader, options);
            _Path.Add(obj_Path);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Path.Count == 0)
          {
            _Path = null;
          }

          break;

        case "relatedArtifact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          RelatedArtifact = new List<RelatedArtifact>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.RelatedArtifact objRelatedArtifact = new fhirCsR5.Models.RelatedArtifact();
            objRelatedArtifact.DeserializeJson(ref reader, options);
            RelatedArtifact.Add(objRelatedArtifact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (RelatedArtifact.Count == 0)
          {
            RelatedArtifact = null;
          }

          break;

        case "summary":
          Summary = reader.GetString();
          break;

        case "_summary":
          _Summary = new fhirCsR5.Models.Element();
          _Summary.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR5.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
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
  /// Code Values for the ArtifactAssessment.content.informationType field
  /// </summary>
  public static class ArtifactAssessmentContentInformationTypeCodes {
    public const string COMMENT = "comment";
    public const string CLASSIFIER = "classifier";
    public const string RATING = "rating";
    public const string CONTAINER = "container";
    public const string RESPONSE = "response";
    public const string CHANGE_REQUEST = "change-request";
  }
  /// <summary>
  /// This Resource provides one or more comments, classifiers or ratings about a Resource and supports attribution and rights management metadata for the added content.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<ArtifactAssessment>))]
  public class ArtifactAssessment : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ArtifactAssessment";
    /// <summary>
    /// The 'date' element may be more recent than the approval date because of minor changes or editorial corrections.
    /// </summary>
    public string ApprovalDate { get; set; }
    /// <summary>
    /// Extension container element for ApprovalDate
    /// </summary>
    public Element _ApprovalDate { get; set; }
    /// <summary>
    /// A reference to a resource, canonical resource, or non-FHIR resource which the comment or assessment is about.
    /// </summary>
    public Reference ArtifactReference { get; set; }
    /// <summary>
    /// A reference to a resource, canonical resource, or non-FHIR resource which the comment or assessment is about.
    /// </summary>
    public string ArtifactCanonical { get; set; }
    /// <summary>
    /// Extension container element for ArtifactCanonical
    /// </summary>
    public Element _ArtifactCanonical { get; set; }
    /// <summary>
    /// A reference to a resource, canonical resource, or non-FHIR resource which the comment or assessment is about.
    /// </summary>
    public string ArtifactUri { get; set; }
    /// <summary>
    /// Extension container element for ArtifactUri
    /// </summary>
    public Element _ArtifactUri { get; set; }
    /// <summary>
    /// Display of or reference to the bibliographic citation of the comment, classifier, or rating.
    /// </summary>
    public Reference CiteAsReference { get; set; }
    /// <summary>
    /// Display of or reference to the bibliographic citation of the comment, classifier, or rating.
    /// </summary>
    public string CiteAsMarkdown { get; set; }
    /// <summary>
    /// Extension container element for CiteAsMarkdown
    /// </summary>
    public Element _CiteAsMarkdown { get; set; }
    /// <summary>
    /// A component comment, classifier, or rating of the artifact.
    /// </summary>
    public List<ArtifactAssessmentContent> Content { get; set; }
    /// <summary>
    /// A copyright statement relating to the artifact assessment and/or its contents. Copyright statements are generally legal restrictions on the use and publishing of the artifact assessment.
    /// </summary>
    public string Copyright { get; set; }
    /// <summary>
    /// Extension container element for Copyright
    /// </summary>
    public Element _Copyright { get; set; }
    /// <summary>
    /// Note that this is not the same as the resource last-modified-date, since the resource may be a secondary representation of the artifact assessment. Additional specific dates may be added as extensions or be found by consulting Provenances associated with past versions of the resource.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// Indicates the disposition of the responsible party to the comment or change request.
    /// </summary>
    public string Disposition { get; set; }
    /// <summary>
    /// Extension container element for Disposition
    /// </summary>
    public Element _Disposition { get; set; }
    /// <summary>
    /// Typically, this is used for identifiers that can go in an HL7 V3 II (instance identifier) data type, and can then identify this activity definition outside of FHIR, where it is not possible to use the logical URI.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// If specified, this date follows the original approval date.
    /// </summary>
    public string LastReviewDate { get; set; }
    /// <summary>
    /// Extension container element for LastReviewDate
    /// </summary>
    public Element _LastReviewDate { get; set; }
    /// <summary>
    /// Indicates the workflow status of the comment or change request.
    /// </summary>
    public string WorkflowStatus { get; set; }
    /// <summary>
    /// Extension container element for WorkflowStatus
    /// </summary>
    public Element _WorkflowStatus { get; set; }
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

      if (CiteAsReference != null)
      {
        writer.WritePropertyName("citeAsReference");
        CiteAsReference.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(CiteAsMarkdown))
      {
        writer.WriteString("citeAsMarkdown", (string)CiteAsMarkdown!);
      }

      if (_CiteAsMarkdown != null)
      {
        writer.WritePropertyName("_citeAsMarkdown");
        _CiteAsMarkdown.SerializeJson(writer, options);
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

      if (!string.IsNullOrEmpty(Copyright))
      {
        writer.WriteString("copyright", (string)Copyright!);
      }

      if (_Copyright != null)
      {
        writer.WritePropertyName("_copyright");
        _Copyright.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ApprovalDate))
      {
        writer.WriteString("approvalDate", (string)ApprovalDate!);
      }

      if (_ApprovalDate != null)
      {
        writer.WritePropertyName("_approvalDate");
        _ApprovalDate.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(LastReviewDate))
      {
        writer.WriteString("lastReviewDate", (string)LastReviewDate!);
      }

      if (_LastReviewDate != null)
      {
        writer.WritePropertyName("_lastReviewDate");
        _LastReviewDate.SerializeJson(writer, options);
      }

      if (ArtifactReference != null)
      {
        writer.WritePropertyName("artifactReference");
        ArtifactReference.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ArtifactCanonical))
      {
        writer.WriteString("artifactCanonical", (string)ArtifactCanonical!);
      }

      if (_ArtifactCanonical != null)
      {
        writer.WritePropertyName("_artifactCanonical");
        _ArtifactCanonical.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ArtifactUri))
      {
        writer.WriteString("artifactUri", (string)ArtifactUri!);
      }

      if (_ArtifactUri != null)
      {
        writer.WritePropertyName("_artifactUri");
        _ArtifactUri.SerializeJson(writer, options);
      }

      if ((Content != null) && (Content.Count != 0))
      {
        writer.WritePropertyName("content");
        writer.WriteStartArray();

        foreach (ArtifactAssessmentContent valContent in Content)
        {
          valContent.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(WorkflowStatus))
      {
        writer.WriteString("workflowStatus", (string)WorkflowStatus!);
      }

      if (_WorkflowStatus != null)
      {
        writer.WritePropertyName("_workflowStatus");
        _WorkflowStatus.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Disposition))
      {
        writer.WriteString("disposition", (string)Disposition!);
      }

      if (_Disposition != null)
      {
        writer.WritePropertyName("_disposition");
        _Disposition.SerializeJson(writer, options);
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
        case "approvalDate":
          ApprovalDate = reader.GetString();
          break;

        case "_approvalDate":
          _ApprovalDate = new fhirCsR5.Models.Element();
          _ApprovalDate.DeserializeJson(ref reader, options);
          break;

        case "artifactReference":
          ArtifactReference = new fhirCsR5.Models.Reference();
          ArtifactReference.DeserializeJson(ref reader, options);
          break;

        case "artifactCanonical":
          ArtifactCanonical = reader.GetString();
          break;

        case "_artifactCanonical":
          _ArtifactCanonical = new fhirCsR5.Models.Element();
          _ArtifactCanonical.DeserializeJson(ref reader, options);
          break;

        case "artifactUri":
          ArtifactUri = reader.GetString();
          break;

        case "_artifactUri":
          _ArtifactUri = new fhirCsR5.Models.Element();
          _ArtifactUri.DeserializeJson(ref reader, options);
          break;

        case "citeAsReference":
          CiteAsReference = new fhirCsR5.Models.Reference();
          CiteAsReference.DeserializeJson(ref reader, options);
          break;

        case "citeAsMarkdown":
          CiteAsMarkdown = reader.GetString();
          break;

        case "_citeAsMarkdown":
          _CiteAsMarkdown = new fhirCsR5.Models.Element();
          _CiteAsMarkdown.DeserializeJson(ref reader, options);
          break;

        case "content":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Content = new List<ArtifactAssessmentContent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.ArtifactAssessmentContent objContent = new fhirCsR5.Models.ArtifactAssessmentContent();
            objContent.DeserializeJson(ref reader, options);
            Content.Add(objContent);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Content.Count == 0)
          {
            Content = null;
          }

          break;

        case "copyright":
          Copyright = reader.GetString();
          break;

        case "_copyright":
          _Copyright = new fhirCsR5.Models.Element();
          _Copyright.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR5.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "disposition":
          Disposition = reader.GetString();
          break;

        case "_disposition":
          _Disposition = new fhirCsR5.Models.Element();
          _Disposition.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Identifier objIdentifier = new fhirCsR5.Models.Identifier();
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

        case "lastReviewDate":
          LastReviewDate = reader.GetString();
          break;

        case "_lastReviewDate":
          _LastReviewDate = new fhirCsR5.Models.Element();
          _LastReviewDate.DeserializeJson(ref reader, options);
          break;

        case "workflowStatus":
          WorkflowStatus = reader.GetString();
          break;

        case "_workflowStatus":
          _WorkflowStatus = new fhirCsR5.Models.Element();
          _WorkflowStatus.DeserializeJson(ref reader, options);
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
  /// <summary>
  /// Code Values for the ArtifactAssessment.disposition field
  /// </summary>
  public static class ArtifactAssessmentDispositionCodes {
    public const string UNRESOLVED = "unresolved";
    public const string NOT_PERSUASIVE = "not-persuasive";
    public const string PERSUASIVE = "persuasive";
    public const string PERSUASIVE_WITH_MODIFICATION = "persuasive-with-modification";
    public const string NOT_PERSUASIVE_WITH_MODIFICATION = "not-persuasive-with-modification";
  }
  /// <summary>
  /// Code Values for the ArtifactAssessment.workflowStatus field
  /// </summary>
  public static class ArtifactAssessmentWorkflowStatusCodes {
    public const string SUBMITTED = "submitted";
    public const string TRIAGED = "triaged";
    public const string WAITING_FOR_INPUT = "waiting-for-input";
    public const string RESOLVED_NO_CHANGE = "resolved-no-change";
    public const string RESOLVED_CHANGE_REQUIRED = "resolved-change-required";
    public const string DEFERRED = "deferred";
    public const string DUPLICATE = "duplicate";
    public const string APPLIED = "applied";
    public const string PUBLISHED = "published";
  }
}
