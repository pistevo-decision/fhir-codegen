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
  /// The ResearchDefinition resource describes the conditional state (population and any exposures being compared within the population) and outcome (if specified) that the knowledge (evidence, assertion, recommendation) is about.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<ResearchDefinition>))]
  public class ResearchDefinition : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ResearchDefinition";
    /// <summary>
    /// The 'date' element may be more recent than the approval date because of minor changes or editorial corrections.
    /// </summary>
    public string ApprovalDate { get; set; }
    /// <summary>
    /// Extension container element for ApprovalDate
    /// </summary>
    public Element _ApprovalDate { get; set; }
    /// <summary>
    /// An individiual or organization primarily involved in the creation and maintenance of the content.
    /// </summary>
    public List<ContactDetail> Author { get; set; }
    /// <summary>
    /// A human-readable string to clarify or explain concepts about the resource.
    /// </summary>
    public List<string> Comment { get; set; }
    /// <summary>
    /// Extension container element for Comment
    /// </summary>
    public List<Element> _Comment { get; set; }
    /// <summary>
    /// May be a web site, an email address, a telephone number, etc.
    /// </summary>
    public List<ContactDetail> Contact { get; set; }
    /// <summary>
    /// A copyright statement relating to the research definition and/or its contents. Copyright statements are generally legal restrictions on the use and publishing of the research definition.
    /// </summary>
    public string Copyright { get; set; }
    /// <summary>
    /// Extension container element for Copyright
    /// </summary>
    public Element _Copyright { get; set; }
    /// <summary>
    /// Note that this is not the same as the resource last-modified-date, since the resource may be a secondary representation of the research definition. Additional specific dates may be added as extensions or be found by consulting Provenances associated with past versions of the resource.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// This description can be used to capture details such as why the research definition was built, comments about misuse, instructions for clinical use and interpretation, literature references, examples from the paper world, etc. It is not a rendering of the research definition as conveyed in the 'text' field of the resource itself. This item SHOULD be populated unless the information is available from context (e.g. the language of the research definition is presumed to be the predominant language in the place the research definition was created).
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// An individual or organization primarily responsible for internal coherence of the content.
    /// </summary>
    public List<ContactDetail> Editor { get; set; }
    /// <summary>
    /// The effective period for a research definition  determines when the content is applicable for usage and is independent of publication and review dates. For example, a measure intended to be used for the year 2016 might be published in 2015.
    /// </summary>
    public Period EffectivePeriod { get; set; }
    /// <summary>
    /// An individual or organization responsible for officially endorsing the content for use in some setting.
    /// </summary>
    public List<ContactDetail> Endorser { get; set; }
    /// <summary>
    /// Allows filtering of research definitions that are appropriate for use versus not.
    /// </summary>
    public bool? Experimental { get; set; }
    /// <summary>
    /// A reference to a ResearchElementDefinition resource that defines the exposure for the research.
    /// </summary>
    public Reference Exposure { get; set; }
    /// <summary>
    /// A reference to a ResearchElementDefinition resource that defines the exposureAlternative for the research.
    /// </summary>
    public Reference ExposureAlternative { get; set; }
    /// <summary>
    /// Typically, this is used for identifiers that can go in an HL7 V3 II (instance identifier) data type, and can then identify this research definition outside of FHIR, where it is not possible to use the logical URI.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// It may be possible for the research definition to be used in jurisdictions other than those for which it was originally designed or intended.
    /// </summary>
    public List<CodeableConcept> Jurisdiction { get; set; }
    /// <summary>
    /// If specified, this date follows the original approval date.
    /// </summary>
    public string LastReviewDate { get; set; }
    /// <summary>
    /// Extension container element for LastReviewDate
    /// </summary>
    public Element _LastReviewDate { get; set; }
    /// <summary>
    /// A reference to a Library resource containing the formal logic used by the ResearchDefinition.
    /// </summary>
    public List<string> Library { get; set; }
    /// <summary>
    /// Extension container element for Library
    /// </summary>
    public List<Element> _Library { get; set; }
    /// <summary>
    /// The name is not expected to be globally unique. The name should be a simple alphanumeric type name to ensure that it is machine-processing friendly.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// A reference to a ResearchElementDefinition resomece that defines the outcome for the research.
    /// </summary>
    public Reference Outcome { get; set; }
    /// <summary>
    /// A reference to a ResearchElementDefinition resource that defines the population for the research.
    /// </summary>
    public Reference Population { get; set; }
    /// <summary>
    /// Usually an organization but may be an individual. The publisher (or steward) of the research definition is the organization or individual primarily responsible for the maintenance and upkeep of the research definition. This is not necessarily the same individual or organization that developed and initially authored the content. The publisher is the primary point of contact for questions or issues with the research definition. This item SHOULD be populated unless the information is available from context.
    /// </summary>
    public string Publisher { get; set; }
    /// <summary>
    /// Extension container element for Publisher
    /// </summary>
    public Element _Publisher { get; set; }
    /// <summary>
    /// This element does not describe the usage of the research definition. Instead, it provides traceability of ''why'' the resource is either needed or ''why'' it is defined as it is.  This may be used to point to source materials or specifications that drove the structure of this research definition.
    /// </summary>
    public string Purpose { get; set; }
    /// <summary>
    /// Extension container element for Purpose
    /// </summary>
    public Element _Purpose { get; set; }
    /// <summary>
    /// Each related artifact is either an attachment, or a reference to another resource, but not both.
    /// </summary>
    public List<RelatedArtifact> RelatedArtifact { get; set; }
    /// <summary>
    /// An individual or organization primarily responsible for review of some aspect of the content.
    /// </summary>
    public List<ContactDetail> Reviewer { get; set; }
    /// <summary>
    /// The short title provides an alternate title for use in informal descriptive contexts where the full, formal title is not necessary.
    /// </summary>
    public string ShortTitle { get; set; }
    /// <summary>
    /// Extension container element for ShortTitle
    /// </summary>
    public Element _ShortTitle { get; set; }
    /// <summary>
    /// Allows filtering of research definitions that are appropriate for use versus not.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The subject of the ResearchDefinition is critical in interpreting the criteria definitions, as the logic in the ResearchDefinitions is evaluated with respect to a particular subject. This corresponds roughly to the notion of a Compartment in that it limits what content is available based on its relationship to the subject. In CQL, this corresponds to the context declaration.
    /// </summary>
    public CodeableConcept SubjectCodeableConcept { get; set; }
    /// <summary>
    /// The subject of the ResearchDefinition is critical in interpreting the criteria definitions, as the logic in the ResearchDefinitions is evaluated with respect to a particular subject. This corresponds roughly to the notion of a Compartment in that it limits what content is available based on its relationship to the subject. In CQL, this corresponds to the context declaration.
    /// </summary>
    public Reference SubjectReference { get; set; }
    /// <summary>
    /// An explanatory or alternate title for the ResearchDefinition giving additional information about its content.
    /// </summary>
    public string Subtitle { get; set; }
    /// <summary>
    /// Extension container element for Subtitle
    /// </summary>
    public Element _Subtitle { get; set; }
    /// <summary>
    /// This name does not need to be machine-processing friendly and may contain punctuation, white-space, etc.
    /// </summary>
    public string Title { get; set; }
    /// <summary>
    /// Extension container element for Title
    /// </summary>
    public Element _Title { get; set; }
    /// <summary>
    /// Descriptive topics related to the content of the ResearchDefinition. Topics provide a high-level categorization grouping types of ResearchDefinitions that can be useful for filtering and searching.
    /// </summary>
    public List<CodeableConcept> Topic { get; set; }
    /// <summary>
    /// Can be a urn:uuid: or a urn:oid: but real http: addresses are preferred.  Multiple instances may share the same URL if they have a distinct version.
    /// The determination of when to create a new version of a resource (same url, new version) vs. defining a new artifact is up to the author.  Considerations for making this decision are found in [Technical and Business Versions](resource.html#versions). 
    /// In some cases, the resource can no longer be found at the stated url, but the url itself cannot change. Implementations can use the [meta.source](resource.html#meta) element to indicate where the current master source of the resource can be found.
    /// </summary>
    public string Url { get; set; }
    /// <summary>
    /// Extension container element for Url
    /// </summary>
    public Element _Url { get; set; }
    /// <summary>
    /// A detailed description, from a clinical perspective, of how the ResearchDefinition is used.
    /// </summary>
    public string Usage { get; set; }
    /// <summary>
    /// Extension container element for Usage
    /// </summary>
    public Element _Usage { get; set; }
    /// <summary>
    /// When multiple useContexts are specified, there is no expectation that all or any of the contexts apply.
    /// </summary>
    public List<UsageContext> UseContext { get; set; }
    /// <summary>
    /// There may be different research definition instances that have the same identifier but different versions.  The version can be appended to the url in a reference to allow a reference to a particular business version of the research definition with the format [url]|[version].
    /// </summary>
    public string Version { get; set; }
    /// <summary>
    /// Extension container element for Version
    /// </summary>
    public Element _Version { get; set; }
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

      if (!string.IsNullOrEmpty(Url))
      {
        writer.WriteString("url", (string)Url!);
      }

      if (_Url != null)
      {
        writer.WritePropertyName("_url");
        _Url.SerializeJson(writer, options);
      }

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

      if (!string.IsNullOrEmpty(Version))
      {
        writer.WriteString("version", (string)Version!);
      }

      if (_Version != null)
      {
        writer.WritePropertyName("_version");
        _Version.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Title))
      {
        writer.WriteString("title", (string)Title!);
      }

      if (_Title != null)
      {
        writer.WritePropertyName("_title");
        _Title.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ShortTitle))
      {
        writer.WriteString("shortTitle", (string)ShortTitle!);
      }

      if (_ShortTitle != null)
      {
        writer.WritePropertyName("_shortTitle");
        _ShortTitle.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Subtitle))
      {
        writer.WriteString("subtitle", (string)Subtitle!);
      }

      if (_Subtitle != null)
      {
        writer.WritePropertyName("_subtitle");
        _Subtitle.SerializeJson(writer, options);
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

      if (Experimental != null)
      {
        writer.WriteBoolean("experimental", (bool)Experimental!);
      }

      if (SubjectCodeableConcept != null)
      {
        writer.WritePropertyName("subjectCodeableConcept");
        SubjectCodeableConcept.SerializeJson(writer, options);
      }

      if (SubjectReference != null)
      {
        writer.WritePropertyName("subjectReference");
        SubjectReference.SerializeJson(writer, options);
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

      if (!string.IsNullOrEmpty(Publisher))
      {
        writer.WriteString("publisher", (string)Publisher!);
      }

      if (_Publisher != null)
      {
        writer.WritePropertyName("_publisher");
        _Publisher.SerializeJson(writer, options);
      }

      if ((Contact != null) && (Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();

        foreach (ContactDetail valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
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

      if ((Comment != null) && (Comment.Count != 0))
      {
        writer.WritePropertyName("comment");
        writer.WriteStartArray();

        foreach (string valComment in Comment)
        {
          writer.WriteStringValue(valComment);
        }

        writer.WriteEndArray();
      }

      if ((_Comment != null) && (_Comment.Count != 0))
      {
        writer.WritePropertyName("_comment");
        writer.WriteStartArray();

        foreach (Element val_Comment in _Comment)
        {
          val_Comment.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((UseContext != null) && (UseContext.Count != 0))
      {
        writer.WritePropertyName("useContext");
        writer.WriteStartArray();

        foreach (UsageContext valUseContext in UseContext)
        {
          valUseContext.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Jurisdiction != null) && (Jurisdiction.Count != 0))
      {
        writer.WritePropertyName("jurisdiction");
        writer.WriteStartArray();

        foreach (CodeableConcept valJurisdiction in Jurisdiction)
        {
          valJurisdiction.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Purpose))
      {
        writer.WriteString("purpose", (string)Purpose!);
      }

      if (_Purpose != null)
      {
        writer.WritePropertyName("_purpose");
        _Purpose.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Usage))
      {
        writer.WriteString("usage", (string)Usage!);
      }

      if (_Usage != null)
      {
        writer.WritePropertyName("_usage");
        _Usage.SerializeJson(writer, options);
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

      if (EffectivePeriod != null)
      {
        writer.WritePropertyName("effectivePeriod");
        EffectivePeriod.SerializeJson(writer, options);
      }

      if ((Topic != null) && (Topic.Count != 0))
      {
        writer.WritePropertyName("topic");
        writer.WriteStartArray();

        foreach (CodeableConcept valTopic in Topic)
        {
          valTopic.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Author != null) && (Author.Count != 0))
      {
        writer.WritePropertyName("author");
        writer.WriteStartArray();

        foreach (ContactDetail valAuthor in Author)
        {
          valAuthor.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Editor != null) && (Editor.Count != 0))
      {
        writer.WritePropertyName("editor");
        writer.WriteStartArray();

        foreach (ContactDetail valEditor in Editor)
        {
          valEditor.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Reviewer != null) && (Reviewer.Count != 0))
      {
        writer.WritePropertyName("reviewer");
        writer.WriteStartArray();

        foreach (ContactDetail valReviewer in Reviewer)
        {
          valReviewer.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Endorser != null) && (Endorser.Count != 0))
      {
        writer.WritePropertyName("endorser");
        writer.WriteStartArray();

        foreach (ContactDetail valEndorser in Endorser)
        {
          valEndorser.SerializeJson(writer, options, true);
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

      if ((Library != null) && (Library.Count != 0))
      {
        writer.WritePropertyName("library");
        writer.WriteStartArray();

        foreach (string valLibrary in Library)
        {
          writer.WriteStringValue(valLibrary);
        }

        writer.WriteEndArray();
      }

      if ((_Library != null) && (_Library.Count != 0))
      {
        writer.WritePropertyName("_library");
        writer.WriteStartArray();

        foreach (Element val_Library in _Library)
        {
          val_Library.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Population != null)
      {
        writer.WritePropertyName("population");
        Population.SerializeJson(writer, options);
      }

      if (Exposure != null)
      {
        writer.WritePropertyName("exposure");
        Exposure.SerializeJson(writer, options);
      }

      if (ExposureAlternative != null)
      {
        writer.WritePropertyName("exposureAlternative");
        ExposureAlternative.SerializeJson(writer, options);
      }

      if (Outcome != null)
      {
        writer.WritePropertyName("outcome");
        Outcome.SerializeJson(writer, options);
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
          _ApprovalDate = new fhirCsR4.Models.Element();
          _ApprovalDate.DeserializeJson(ref reader, options);
          break;

        case "author":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Author = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.ContactDetail objAuthor = new fhirCsR4.Models.ContactDetail();
            objAuthor.DeserializeJson(ref reader, options);
            Author.Add(objAuthor);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Author.Count == 0)
          {
            Author = null;
          }

          break;

        case "comment":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Comment = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Comment.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Comment.Count == 0)
          {
            Comment = null;
          }

          break;

        case "_comment":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Comment = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Element obj_Comment = new fhirCsR4.Models.Element();
            obj_Comment.DeserializeJson(ref reader, options);
            _Comment.Add(obj_Comment);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Comment.Count == 0)
          {
            _Comment = null;
          }

          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.ContactDetail objContact = new fhirCsR4.Models.ContactDetail();
            objContact.DeserializeJson(ref reader, options);
            Contact.Add(objContact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Contact.Count == 0)
          {
            Contact = null;
          }

          break;

        case "copyright":
          Copyright = reader.GetString();
          break;

        case "_copyright":
          _Copyright = new fhirCsR4.Models.Element();
          _Copyright.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR4.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR4.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "editor":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Editor = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.ContactDetail objEditor = new fhirCsR4.Models.ContactDetail();
            objEditor.DeserializeJson(ref reader, options);
            Editor.Add(objEditor);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Editor.Count == 0)
          {
            Editor = null;
          }

          break;

        case "effectivePeriod":
          EffectivePeriod = new fhirCsR4.Models.Period();
          EffectivePeriod.DeserializeJson(ref reader, options);
          break;

        case "endorser":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Endorser = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.ContactDetail objEndorser = new fhirCsR4.Models.ContactDetail();
            objEndorser.DeserializeJson(ref reader, options);
            Endorser.Add(objEndorser);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Endorser.Count == 0)
          {
            Endorser = null;
          }

          break;

        case "experimental":
          Experimental = reader.GetBoolean();
          break;

        case "exposure":
          Exposure = new fhirCsR4.Models.Reference();
          Exposure.DeserializeJson(ref reader, options);
          break;

        case "exposureAlternative":
          ExposureAlternative = new fhirCsR4.Models.Reference();
          ExposureAlternative.DeserializeJson(ref reader, options);
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

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CodeableConcept objJurisdiction = new fhirCsR4.Models.CodeableConcept();
            objJurisdiction.DeserializeJson(ref reader, options);
            Jurisdiction.Add(objJurisdiction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Jurisdiction.Count == 0)
          {
            Jurisdiction = null;
          }

          break;

        case "lastReviewDate":
          LastReviewDate = reader.GetString();
          break;

        case "_lastReviewDate":
          _LastReviewDate = new fhirCsR4.Models.Element();
          _LastReviewDate.DeserializeJson(ref reader, options);
          break;

        case "library":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Library = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Library.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Library.Count == 0)
          {
            Library = null;
          }

          break;

        case "_library":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Library = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Element obj_Library = new fhirCsR4.Models.Element();
            obj_Library.DeserializeJson(ref reader, options);
            _Library.Add(obj_Library);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Library.Count == 0)
          {
            _Library = null;
          }

          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR4.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "outcome":
          Outcome = new fhirCsR4.Models.Reference();
          Outcome.DeserializeJson(ref reader, options);
          break;

        case "population":
          Population = new fhirCsR4.Models.Reference();
          Population.DeserializeJson(ref reader, options);
          break;

        case "publisher":
          Publisher = reader.GetString();
          break;

        case "_publisher":
          _Publisher = new fhirCsR4.Models.Element();
          _Publisher.DeserializeJson(ref reader, options);
          break;

        case "purpose":
          Purpose = reader.GetString();
          break;

        case "_purpose":
          _Purpose = new fhirCsR4.Models.Element();
          _Purpose.DeserializeJson(ref reader, options);
          break;

        case "relatedArtifact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          RelatedArtifact = new List<RelatedArtifact>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.RelatedArtifact objRelatedArtifact = new fhirCsR4.Models.RelatedArtifact();
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

        case "reviewer":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Reviewer = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.ContactDetail objReviewer = new fhirCsR4.Models.ContactDetail();
            objReviewer.DeserializeJson(ref reader, options);
            Reviewer.Add(objReviewer);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Reviewer.Count == 0)
          {
            Reviewer = null;
          }

          break;

        case "shortTitle":
          ShortTitle = reader.GetString();
          break;

        case "_shortTitle":
          _ShortTitle = new fhirCsR4.Models.Element();
          _ShortTitle.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR4.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subjectCodeableConcept":
          SubjectCodeableConcept = new fhirCsR4.Models.CodeableConcept();
          SubjectCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "subjectReference":
          SubjectReference = new fhirCsR4.Models.Reference();
          SubjectReference.DeserializeJson(ref reader, options);
          break;

        case "subtitle":
          Subtitle = reader.GetString();
          break;

        case "_subtitle":
          _Subtitle = new fhirCsR4.Models.Element();
          _Subtitle.DeserializeJson(ref reader, options);
          break;

        case "title":
          Title = reader.GetString();
          break;

        case "_title":
          _Title = new fhirCsR4.Models.Element();
          _Title.DeserializeJson(ref reader, options);
          break;

        case "topic":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Topic = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CodeableConcept objTopic = new fhirCsR4.Models.CodeableConcept();
            objTopic.DeserializeJson(ref reader, options);
            Topic.Add(objTopic);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Topic.Count == 0)
          {
            Topic = null;
          }

          break;

        case "url":
          Url = reader.GetString();
          break;

        case "_url":
          _Url = new fhirCsR4.Models.Element();
          _Url.DeserializeJson(ref reader, options);
          break;

        case "usage":
          Usage = reader.GetString();
          break;

        case "_usage":
          _Usage = new fhirCsR4.Models.Element();
          _Usage.DeserializeJson(ref reader, options);
          break;

        case "useContext":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          UseContext = new List<UsageContext>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.UsageContext objUseContext = new fhirCsR4.Models.UsageContext();
            objUseContext.DeserializeJson(ref reader, options);
            UseContext.Add(objUseContext);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (UseContext.Count == 0)
          {
            UseContext = null;
          }

          break;

        case "version":
          Version = reader.GetString();
          break;

        case "_version":
          _Version = new fhirCsR4.Models.Element();
          _Version.DeserializeJson(ref reader, options);
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
  /// Code Values for the ResearchDefinition.status field
  /// </summary>
  public static class ResearchDefinitionStatusCodes {
    public const string DRAFT = "draft";
    public const string ACTIVE = "active";
    public const string RETIRED = "retired";
    public const string UNKNOWN = "unknown";
  }
}
