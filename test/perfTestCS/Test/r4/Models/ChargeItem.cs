// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using Fhir.R4.Serialization;

namespace Fhir.R4.Models
{
  /// <summary>
  /// Indicates who or what performed or participated in the charged service.
  /// </summary>
  [JsonConverter(typeof(Fhir.R4.Serialization.JsonStreamComponentConverter<ChargeItemPerformer>))]
  public class ChargeItemPerformer : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The device, practitioner, etc. who performed or participated in the service.
    /// </summary>
    public Reference Actor { get; set; }
    /// <summary>
    /// Describes the type of performance or participation(e.g. primary surgeon, anesthesiologiest, etc.).
    /// </summary>
    public CodeableConcept Function { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }

      ((Fhir.R4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Function != null)
      {
        writer.WritePropertyName("function");
        Function.SerializeJson(writer, options);
      }

      writer.WritePropertyName("actor");
      Actor.SerializeJson(writer, options);

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
        case "actor":
          Actor = new Fhir.R4.Models.Reference();
          Actor.DeserializeJson(ref reader, options);
          break;

        case "function":
          Function = new Fhir.R4.Models.CodeableConcept();
          Function.DeserializeJson(ref reader, options);
          break;

        default:
          ((Fhir.R4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// The resource ChargeItem describes the provision of healthcare provider products for a certain patient, therefore referring not only to the product, but containing in addition details of the provision, like date, time, amounts and participating organizations and persons. Main Usage of the ChargeItem is to enable the billing process and internal cost allocation.
  /// </summary>
  [JsonConverter(typeof(Fhir.R4.Serialization.JsonStreamComponentConverter<ChargeItem>))]
  public class ChargeItem : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public string ResourceType => "ChargeItem";
    /// <summary>
    /// Systems posting the ChargeItems might not always be able to determine, which accounts the Items need to be places into. It is up to the postprocessing Financial System to apply internal rules to decide based on the Encounter/EpisodeOfCare/Patient/Coverage context and the type of ChargeItem, which Account is appropriate.
    /// </summary>
    public List<Reference> Account { get; set; }
    /// <summary>
    /// Only used if not implicit in code found in Condition.code. If the use case requires attributes from the BodySite resource (e.g. to identify and track separately) then use the standard extension [bodySite](extension-bodysite.html).  May be a summary code, or a reference to a very precise definition of the location, or both.
    /// </summary>
    public List<CodeableConcept> Bodysite { get; set; }
    /// <summary>
    /// A code that identifies the charge, like a billing code.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// The encounter or episode of care that establishes the context for this event.
    /// </summary>
    public Reference Context { get; set; }
    /// <summary>
    /// The costCenter could either be given as a reference to an Organization(Role) resource or as the identifier of the cost center determined by Reference.identifier.value and Reference.identifier.system, depending on use case requirements.
    /// </summary>
    public Reference CostCenter { get; set; }
    /// <summary>
    /// References the source of pricing information, rules of application for the code this ChargeItem uses.
    /// </summary>
    public List<string> DefinitionCanonical { get; set; }
    /// <summary>
    /// Extension container element for DefinitionCanonical
    /// </summary>
    public List<Element> _DefinitionCanonical { get; set; }
    /// <summary>
    /// References the (external) source of pricing information, rules of application for the code this ChargeItem uses.
    /// </summary>
    public List<string> DefinitionUri { get; set; }
    /// <summary>
    /// Extension container element for DefinitionUri
    /// </summary>
    public List<Element> _DefinitionUri { get; set; }
    /// <summary>
    /// The actual date when the service associated with the charge has been rendered is captured in occurrence[x].
    /// </summary>
    public string EnteredDate { get; set; }
    /// <summary>
    /// Extension container element for EnteredDate
    /// </summary>
    public Element _EnteredDate { get; set; }
    /// <summary>
    /// The enterer is also the person considered responsible for factor/price overrides if applicable.
    /// </summary>
    public Reference Enterer { get; set; }
    /// <summary>
    /// There is no reason to carry the factor in the instance of a ChargeItem unless special circumstances require a manual override. The factors are usually defined by a set of rules in a back catalogue of the billing codes  (see ChargeItem.definition). Derived profiles may require a ChargeItem.overrideReason to be provided if either factor or price are manually overridden.
    /// </summary>
    public decimal? FactorOverride { get; set; }
    /// <summary>
    /// Extension container element for FactorOverride
    /// </summary>
    public Element _FactorOverride { get; set; }
    /// <summary>
    /// Identifiers assigned to this event performer or other systems.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Comments made about the event by the performer, subject or other participants.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// The list of types may be constrained as appropriate for the type of charge item.
    /// </summary>
    public string OccurrenceDateTime { get; set; }
    /// <summary>
    /// Extension container element for OccurrenceDateTime
    /// </summary>
    public Element _OccurrenceDateTime { get; set; }
    /// <summary>
    /// The list of types may be constrained as appropriate for the type of charge item.
    /// </summary>
    public Period OccurrencePeriod { get; set; }
    /// <summary>
    /// The list of types may be constrained as appropriate for the type of charge item.
    /// </summary>
    public Timing OccurrenceTiming { get; set; }
    /// <summary>
    /// Derived Profiles may choose to add invariants requiring this field to be populated if either priceOverride or factorOverride have been filled.
    /// </summary>
    public string OverrideReason { get; set; }
    /// <summary>
    /// Extension container element for OverrideReason
    /// </summary>
    public Element _OverrideReason { get; set; }
    /// <summary>
    /// ChargeItems can be grouped to larger ChargeItems covering the whole set.
    /// </summary>
    public List<Reference> PartOf { get; set; }
    /// <summary>
    /// Indicates who or what performed or participated in the charged service.
    /// </summary>
    public List<ChargeItemPerformer> Performer { get; set; }
    /// <summary>
    /// Practitioners and Devices can be associated with multiple organizations. It has to be made clear, on behalf of which Organization the services have been rendered.
    /// </summary>
    public Reference PerformingOrganization { get; set; }
    /// <summary>
    /// There is no reason to carry the price in the instance of a ChargeItem unless circumstances require a manual override. The list prices or are usually defined in a back catalogue of the billing codes  (see ChargeItem.definition). Derived profiles may require a ChargeItem.overrideReason to be provided if either factor or price are manually overridden.
    /// </summary>
    public Money PriceOverride { get; set; }
    /// <summary>
    /// Identifies the device, food, drug or other product being charged either by type code or reference to an instance.
    /// </summary>
    public Reference ProductReference { get; set; }
    /// <summary>
    /// Identifies the device, food, drug or other product being charged either by type code or reference to an instance.
    /// </summary>
    public CodeableConcept ProductCodeableConcept { get; set; }
    /// <summary>
    /// In many cases this may just be a value, if the underlying units are implicit in the definition of the charge item code.
    /// </summary>
    public Quantity Quantity { get; set; }
    /// <summary>
    /// If the application of the charge item requires a reason to be given, it can be captured here. Textual reasons can be captured using reasonCode.text.
    /// </summary>
    public List<CodeableConcept> Reason { get; set; }
    /// <summary>
    /// The rendered Service might not be associated with a Request. This property indicates which Organization requested the services to be rendered. (In many cases, this may just be the Department associated with the Encounter.location).
    /// </summary>
    public Reference RequestingOrganization { get; set; }
    /// <summary>
    /// Indicated the rendered service that caused this charge.
    /// </summary>
    public List<Reference> Service { get; set; }
    /// <summary>
    /// Unknown does not represent "other" - one of the defined statuses must apply.  Unknown is used when the authoring system is not sure what the current status is.
    /// This element is labeled as a modifier because the status contains the code entered-in-error that marks the charge item as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The individual or set of individuals the action is being or was performed on.
    /// </summary>
    public Reference Subject { get; set; }
    /// <summary>
    /// Further information supporting this charge.
    /// </summary>
    public List<Reference> SupportingInformation { get; set; }
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


      ((Fhir.R4.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if ((DefinitionUri != null) && (DefinitionUri.Count != 0))
      {
        writer.WritePropertyName("definitionUri");
        writer.WriteStartArray();

        foreach (string valDefinitionUri in DefinitionUri)
        {
          writer.WriteStringValue(valDefinitionUri);
        }

        writer.WriteEndArray();
      }

      if ((_DefinitionUri != null) && (_DefinitionUri.Count != 0))
      {
        writer.WritePropertyName("_definitionUri");
        writer.WriteStartArray();

        foreach (Element val_DefinitionUri in _DefinitionUri)
        {
          val_DefinitionUri.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((DefinitionCanonical != null) && (DefinitionCanonical.Count != 0))
      {
        writer.WritePropertyName("definitionCanonical");
        writer.WriteStartArray();

        foreach (string valDefinitionCanonical in DefinitionCanonical)
        {
          writer.WriteStringValue(valDefinitionCanonical);
        }

        writer.WriteEndArray();
      }

      if ((_DefinitionCanonical != null) && (_DefinitionCanonical.Count != 0))
      {
        writer.WritePropertyName("_definitionCanonical");
        writer.WriteStartArray();

        foreach (Element val_DefinitionCanonical in _DefinitionCanonical)
        {
          val_DefinitionCanonical.SerializeJson(writer, options, true);
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

      if ((PartOf != null) && (PartOf.Count != 0))
      {
        writer.WritePropertyName("partOf");
        writer.WriteStartArray();

        foreach (Reference valPartOf in PartOf)
        {
          valPartOf.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      writer.WritePropertyName("code");
      Code.SerializeJson(writer, options);

      writer.WritePropertyName("subject");
      Subject.SerializeJson(writer, options);

      if (Context != null)
      {
        writer.WritePropertyName("context");
        Context.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(OccurrenceDateTime))
      {
        writer.WriteString("occurrenceDateTime", (string)OccurrenceDateTime!);
      }

      if (_OccurrenceDateTime != null)
      {
        writer.WritePropertyName("_occurrenceDateTime");
        _OccurrenceDateTime.SerializeJson(writer, options);
      }

      if (OccurrencePeriod != null)
      {
        writer.WritePropertyName("occurrencePeriod");
        OccurrencePeriod.SerializeJson(writer, options);
      }

      if (OccurrenceTiming != null)
      {
        writer.WritePropertyName("occurrenceTiming");
        OccurrenceTiming.SerializeJson(writer, options);
      }

      if ((Performer != null) && (Performer.Count != 0))
      {
        writer.WritePropertyName("performer");
        writer.WriteStartArray();

        foreach (ChargeItemPerformer valPerformer in Performer)
        {
          valPerformer.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (PerformingOrganization != null)
      {
        writer.WritePropertyName("performingOrganization");
        PerformingOrganization.SerializeJson(writer, options);
      }

      if (RequestingOrganization != null)
      {
        writer.WritePropertyName("requestingOrganization");
        RequestingOrganization.SerializeJson(writer, options);
      }

      if (CostCenter != null)
      {
        writer.WritePropertyName("costCenter");
        CostCenter.SerializeJson(writer, options);
      }

      if (Quantity != null)
      {
        writer.WritePropertyName("quantity");
        Quantity.SerializeJson(writer, options);
      }

      if ((Bodysite != null) && (Bodysite.Count != 0))
      {
        writer.WritePropertyName("bodysite");
        writer.WriteStartArray();

        foreach (CodeableConcept valBodysite in Bodysite)
        {
          valBodysite.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (FactorOverride != null)
      {
        writer.WriteNumber("factorOverride", (decimal)FactorOverride!);
      }

      if (_FactorOverride != null)
      {
        writer.WritePropertyName("_factorOverride");
        _FactorOverride.SerializeJson(writer, options);
      }

      if (PriceOverride != null)
      {
        writer.WritePropertyName("priceOverride");
        PriceOverride.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(OverrideReason))
      {
        writer.WriteString("overrideReason", (string)OverrideReason!);
      }

      if (_OverrideReason != null)
      {
        writer.WritePropertyName("_overrideReason");
        _OverrideReason.SerializeJson(writer, options);
      }

      if (Enterer != null)
      {
        writer.WritePropertyName("enterer");
        Enterer.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(EnteredDate))
      {
        writer.WriteString("enteredDate", (string)EnteredDate!);
      }

      if (_EnteredDate != null)
      {
        writer.WritePropertyName("_enteredDate");
        _EnteredDate.SerializeJson(writer, options);
      }

      if ((Reason != null) && (Reason.Count != 0))
      {
        writer.WritePropertyName("reason");
        writer.WriteStartArray();

        foreach (CodeableConcept valReason in Reason)
        {
          valReason.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Service != null) && (Service.Count != 0))
      {
        writer.WritePropertyName("service");
        writer.WriteStartArray();

        foreach (Reference valService in Service)
        {
          valService.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (ProductReference != null)
      {
        writer.WritePropertyName("productReference");
        ProductReference.SerializeJson(writer, options);
      }

      if (ProductCodeableConcept != null)
      {
        writer.WritePropertyName("productCodeableConcept");
        ProductCodeableConcept.SerializeJson(writer, options);
      }

      if ((Account != null) && (Account.Count != 0))
      {
        writer.WritePropertyName("account");
        writer.WriteStartArray();

        foreach (Reference valAccount in Account)
        {
          valAccount.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Note != null) && (Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();

        foreach (Annotation valNote in Note)
        {
          valNote.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((SupportingInformation != null) && (SupportingInformation.Count != 0))
      {
        writer.WritePropertyName("supportingInformation");
        writer.WriteStartArray();

        foreach (Reference valSupportingInformation in SupportingInformation)
        {
          valSupportingInformation.SerializeJson(writer, options, true);
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
        case "account":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Account = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Reference objAccount = new Fhir.R4.Models.Reference();
            objAccount.DeserializeJson(ref reader, options);
            Account.Add(objAccount);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Account.Count == 0)
          {
            Account = null;
          }

          break;

        case "bodysite":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Bodysite = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.CodeableConcept objBodysite = new Fhir.R4.Models.CodeableConcept();
            objBodysite.DeserializeJson(ref reader, options);
            Bodysite.Add(objBodysite);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Bodysite.Count == 0)
          {
            Bodysite = null;
          }

          break;

        case "code":
          Code = new Fhir.R4.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "context":
          Context = new Fhir.R4.Models.Reference();
          Context.DeserializeJson(ref reader, options);
          break;

        case "costCenter":
          CostCenter = new Fhir.R4.Models.Reference();
          CostCenter.DeserializeJson(ref reader, options);
          break;

        case "definitionCanonical":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DefinitionCanonical = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            DefinitionCanonical.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DefinitionCanonical.Count == 0)
          {
            DefinitionCanonical = null;
          }

          break;

        case "_definitionCanonical":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _DefinitionCanonical = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Element obj_DefinitionCanonical = new Fhir.R4.Models.Element();
            obj_DefinitionCanonical.DeserializeJson(ref reader, options);
            _DefinitionCanonical.Add(obj_DefinitionCanonical);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_DefinitionCanonical.Count == 0)
          {
            _DefinitionCanonical = null;
          }

          break;

        case "definitionUri":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DefinitionUri = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            DefinitionUri.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DefinitionUri.Count == 0)
          {
            DefinitionUri = null;
          }

          break;

        case "_definitionUri":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _DefinitionUri = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Element obj_DefinitionUri = new Fhir.R4.Models.Element();
            obj_DefinitionUri.DeserializeJson(ref reader, options);
            _DefinitionUri.Add(obj_DefinitionUri);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_DefinitionUri.Count == 0)
          {
            _DefinitionUri = null;
          }

          break;

        case "enteredDate":
          EnteredDate = reader.GetString();
          break;

        case "_enteredDate":
          _EnteredDate = new Fhir.R4.Models.Element();
          _EnteredDate.DeserializeJson(ref reader, options);
          break;

        case "enterer":
          Enterer = new Fhir.R4.Models.Reference();
          Enterer.DeserializeJson(ref reader, options);
          break;

        case "factorOverride":
          FactorOverride = reader.GetDecimal();
          break;

        case "_factorOverride":
          _FactorOverride = new Fhir.R4.Models.Element();
          _FactorOverride.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Identifier objIdentifier = new Fhir.R4.Models.Identifier();
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

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Annotation objNote = new Fhir.R4.Models.Annotation();
            objNote.DeserializeJson(ref reader, options);
            Note.Add(objNote);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Note.Count == 0)
          {
            Note = null;
          }

          break;

        case "occurrenceDateTime":
          OccurrenceDateTime = reader.GetString();
          break;

        case "_occurrenceDateTime":
          _OccurrenceDateTime = new Fhir.R4.Models.Element();
          _OccurrenceDateTime.DeserializeJson(ref reader, options);
          break;

        case "occurrencePeriod":
          OccurrencePeriod = new Fhir.R4.Models.Period();
          OccurrencePeriod.DeserializeJson(ref reader, options);
          break;

        case "occurrenceTiming":
          OccurrenceTiming = new Fhir.R4.Models.Timing();
          OccurrenceTiming.DeserializeJson(ref reader, options);
          break;

        case "overrideReason":
          OverrideReason = reader.GetString();
          break;

        case "_overrideReason":
          _OverrideReason = new Fhir.R4.Models.Element();
          _OverrideReason.DeserializeJson(ref reader, options);
          break;

        case "partOf":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          PartOf = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Reference objPartOf = new Fhir.R4.Models.Reference();
            objPartOf.DeserializeJson(ref reader, options);
            PartOf.Add(objPartOf);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (PartOf.Count == 0)
          {
            PartOf = null;
          }

          break;

        case "performer":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Performer = new List<ChargeItemPerformer>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.ChargeItemPerformer objPerformer = new Fhir.R4.Models.ChargeItemPerformer();
            objPerformer.DeserializeJson(ref reader, options);
            Performer.Add(objPerformer);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Performer.Count == 0)
          {
            Performer = null;
          }

          break;

        case "performingOrganization":
          PerformingOrganization = new Fhir.R4.Models.Reference();
          PerformingOrganization.DeserializeJson(ref reader, options);
          break;

        case "priceOverride":
          PriceOverride = new Fhir.R4.Models.Money();
          PriceOverride.DeserializeJson(ref reader, options);
          break;

        case "productReference":
          ProductReference = new Fhir.R4.Models.Reference();
          ProductReference.DeserializeJson(ref reader, options);
          break;

        case "productCodeableConcept":
          ProductCodeableConcept = new Fhir.R4.Models.CodeableConcept();
          ProductCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "quantity":
          Quantity = new Fhir.R4.Models.Quantity();
          Quantity.DeserializeJson(ref reader, options);
          break;

        case "reason":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Reason = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.CodeableConcept objReason = new Fhir.R4.Models.CodeableConcept();
            objReason.DeserializeJson(ref reader, options);
            Reason.Add(objReason);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Reason.Count == 0)
          {
            Reason = null;
          }

          break;

        case "requestingOrganization":
          RequestingOrganization = new Fhir.R4.Models.Reference();
          RequestingOrganization.DeserializeJson(ref reader, options);
          break;

        case "service":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Service = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Reference objService = new Fhir.R4.Models.Reference();
            objService.DeserializeJson(ref reader, options);
            Service.Add(objService);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Service.Count == 0)
          {
            Service = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new Fhir.R4.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new Fhir.R4.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
          break;

        case "supportingInformation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          SupportingInformation = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Reference objSupportingInformation = new Fhir.R4.Models.Reference();
            objSupportingInformation.DeserializeJson(ref reader, options);
            SupportingInformation.Add(objSupportingInformation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (SupportingInformation.Count == 0)
          {
            SupportingInformation = null;
          }

          break;

        default:
          ((Fhir.R4.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
