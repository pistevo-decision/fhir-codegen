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
  /// For example may be used to identify a class of coverage or employer group, Policy, Plan.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<CoverageClass>))]
  public class CoverageClass : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// A short description for the class.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// The type of classification for which an insurer-specific class label or number and optional name is provided, for example may be used to identify a class of coverage or employer group, Policy, Plan.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// For example, the Group or Plan number.
    /// </summary>
    public string Value { get; set; }
    /// <summary>
    /// Extension container element for Value
    /// </summary>
    public Element _Value { get; set; }
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

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Value))
      {
        writer.WriteString("value", (string)Value!);
      }

      if (_Value != null)
      {
        writer.WritePropertyName("_value");
        _Value.SerializeJson(writer, options);
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
        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR4.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR4.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        case "value":
          Value = reader.GetString();
          break;

        case "_value":
          _Value = new fhirCsR4.Models.Element();
          _Value.DeserializeJson(ref reader, options);
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
  /// A suite of codes indicating exceptions or reductions to patient costs and their effective periods.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<CoverageCostToBeneficiaryException>))]
  public class CoverageCostToBeneficiaryException : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The timeframe during when the exception is in force.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// The code for the specific exception.
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
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
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
        case "period":
          Period = new fhirCsR4.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR4.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
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
  /// For example by knowing the patient visit co-pay, the provider can collect the amount prior to undertaking treatment.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<CoverageCostToBeneficiary>))]
  public class CoverageCostToBeneficiary : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// A suite of codes indicating exceptions or reductions to patient costs and their effective periods.
    /// </summary>
    public List<CoverageCostToBeneficiaryException> Exception { get; set; }
    /// <summary>
    /// For example visit, specialist visits, emergency, inpatient care, etc.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// Amount may be expressed as a percentage of the service/product cost or a fixed amount of currency.
    /// </summary>
    public Quantity ValueQuantity { get; set; }
    /// <summary>
    /// Amount may be expressed as a percentage of the service/product cost or a fixed amount of currency.
    /// </summary>
    public Money ValueMoney { get; set; }
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

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (ValueQuantity != null)
      {
        writer.WritePropertyName("valueQuantity");
        ValueQuantity.SerializeJson(writer, options);
      }

      if (ValueMoney != null)
      {
        writer.WritePropertyName("valueMoney");
        ValueMoney.SerializeJson(writer, options);
      }

      if ((Exception != null) && (Exception.Count != 0))
      {
        writer.WritePropertyName("exception");
        writer.WriteStartArray();

        foreach (CoverageCostToBeneficiaryException valException in Exception)
        {
          valException.SerializeJson(writer, options, true);
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
        case "exception":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Exception = new List<CoverageCostToBeneficiaryException>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CoverageCostToBeneficiaryException objException = new fhirCsR4.Models.CoverageCostToBeneficiaryException();
            objException.DeserializeJson(ref reader, options);
            Exception.Add(objException);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Exception.Count == 0)
          {
            Exception = null;
          }

          break;

        case "type":
          Type = new fhirCsR4.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        case "valueQuantity":
          ValueQuantity = new fhirCsR4.Models.Quantity();
          ValueQuantity.DeserializeJson(ref reader, options);
          break;

        case "valueMoney":
          ValueMoney = new fhirCsR4.Models.Money();
          ValueMoney.DeserializeJson(ref reader, options);
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
  /// Financial instrument which may be used to reimburse or pay for health care products and services. Includes both insurance and self-payment.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<Coverage>))]
  public class Coverage : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Coverage";
    /// <summary>
    /// The party who benefits from the insurance coverage; the patient when products and/or services are provided.
    /// </summary>
    public Reference Beneficiary { get; set; }
    /// <summary>
    /// For example may be used to identify a class of coverage or employer group, Policy, Plan.
    /// </summary>
    public List<CoverageClass> Class { get; set; }
    /// <summary>
    /// The policy(s) which constitute this insurance coverage.
    /// </summary>
    public List<Reference> Contract { get; set; }
    /// <summary>
    /// For example by knowing the patient visit co-pay, the provider can collect the amount prior to undertaking treatment.
    /// </summary>
    public List<CoverageCostToBeneficiary> CostToBeneficiary { get; set; }
    /// <summary>
    /// Periodically the member number is constructed from the subscriberId and the dependant number.
    /// </summary>
    public string Dependent { get; set; }
    /// <summary>
    /// Extension container element for Dependent
    /// </summary>
    public Element _Dependent { get; set; }
    /// <summary>
    /// The main (and possibly only) identifier for the coverage - often referred to as a Member Id, Certificate number, Personal Health Number or Case ID. May be constructed as the concatenation of the Coverage.SubscriberID and the Coverage.dependant.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The insurer-specific identifier for the insurer-defined network of providers to which the beneficiary may seek treatment which will be covered at the 'in-network' rate, otherwise 'out of network' terms and conditions apply.
    /// </summary>
    public string Network { get; set; }
    /// <summary>
    /// Extension container element for Network
    /// </summary>
    public Element _Network { get; set; }
    /// <summary>
    /// The order of applicability of this coverage relative to other coverages which are currently in force. Note, there may be gaps in the numbering and this does not imply primary, secondary etc. as the specific positioning of coverages depends upon the episode of care.
    /// </summary>
    public uint? Order { get; set; }
    /// <summary>
    /// May provide multiple identifiers such as insurance company identifier or business identifier (BIN number).
    /// For selfpay it may provide multiple paying persons and/or organizations.
    /// </summary>
    public List<Reference> Payor { get; set; }
    /// <summary>
    /// Time period during which the coverage is in force. A missing start date indicates the start date isn't known, a missing end date means the coverage is continuing to be in force.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// For example: may be an individual, corporation or the subscriber's employer.
    /// </summary>
    public Reference PolicyHolder { get; set; }
    /// <summary>
    /// Typically, an individual uses policies which are theirs (relationship='self') before policies owned by others.
    /// </summary>
    public CodeableConcept Relationship { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains the code entered-in-error that marks the coverage as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Typically, automotive and worker's compensation policies would be flagged with 'subrogation=true' to enable healthcare payors to collect against accident claims.
    /// </summary>
    public bool? Subrogation { get; set; }
    /// <summary>
    /// May be self or a parent in the case of dependants.
    /// </summary>
    public Reference Subscriber { get; set; }
    /// <summary>
    /// The insurer assigned ID for the Subscriber.
    /// </summary>
    public string SubscriberId { get; set; }
    /// <summary>
    /// Extension container element for SubscriberId
    /// </summary>
    public Element _SubscriberId { get; set; }
    /// <summary>
    /// The type of coverage: social program, medical plan, accident coverage (workers compensation, auto), group health or payment by an individual or organization.
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

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (PolicyHolder != null)
      {
        writer.WritePropertyName("policyHolder");
        PolicyHolder.SerializeJson(writer, options);
      }

      if (Subscriber != null)
      {
        writer.WritePropertyName("subscriber");
        Subscriber.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(SubscriberId))
      {
        writer.WriteString("subscriberId", (string)SubscriberId!);
      }

      if (_SubscriberId != null)
      {
        writer.WritePropertyName("_subscriberId");
        _SubscriberId.SerializeJson(writer, options);
      }

      if (Beneficiary != null)
      {
        writer.WritePropertyName("beneficiary");
        Beneficiary.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Dependent))
      {
        writer.WriteString("dependent", (string)Dependent!);
      }

      if (_Dependent != null)
      {
        writer.WritePropertyName("_dependent");
        _Dependent.SerializeJson(writer, options);
      }

      if (Relationship != null)
      {
        writer.WritePropertyName("relationship");
        Relationship.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if ((Payor != null) && (Payor.Count != 0))
      {
        writer.WritePropertyName("payor");
        writer.WriteStartArray();

        foreach (Reference valPayor in Payor)
        {
          valPayor.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Class != null) && (Class.Count != 0))
      {
        writer.WritePropertyName("class");
        writer.WriteStartArray();

        foreach (CoverageClass valClass in Class)
        {
          valClass.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Order != null)
      {
        writer.WriteNumber("order", (uint)Order!);
      }

      if (!string.IsNullOrEmpty(Network))
      {
        writer.WriteString("network", (string)Network!);
      }

      if (_Network != null)
      {
        writer.WritePropertyName("_network");
        _Network.SerializeJson(writer, options);
      }

      if ((CostToBeneficiary != null) && (CostToBeneficiary.Count != 0))
      {
        writer.WritePropertyName("costToBeneficiary");
        writer.WriteStartArray();

        foreach (CoverageCostToBeneficiary valCostToBeneficiary in CostToBeneficiary)
        {
          valCostToBeneficiary.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Subrogation != null)
      {
        writer.WriteBoolean("subrogation", (bool)Subrogation!);
      }

      if ((Contract != null) && (Contract.Count != 0))
      {
        writer.WritePropertyName("contract");
        writer.WriteStartArray();

        foreach (Reference valContract in Contract)
        {
          valContract.SerializeJson(writer, options, true);
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
        case "beneficiary":
          Beneficiary = new fhirCsR4.Models.Reference();
          Beneficiary.DeserializeJson(ref reader, options);
          break;

        case "class":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Class = new List<CoverageClass>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CoverageClass objClass = new fhirCsR4.Models.CoverageClass();
            objClass.DeserializeJson(ref reader, options);
            Class.Add(objClass);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Class.Count == 0)
          {
            Class = null;
          }

          break;

        case "contract":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contract = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Reference objContract = new fhirCsR4.Models.Reference();
            objContract.DeserializeJson(ref reader, options);
            Contract.Add(objContract);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Contract.Count == 0)
          {
            Contract = null;
          }

          break;

        case "costToBeneficiary":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          CostToBeneficiary = new List<CoverageCostToBeneficiary>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CoverageCostToBeneficiary objCostToBeneficiary = new fhirCsR4.Models.CoverageCostToBeneficiary();
            objCostToBeneficiary.DeserializeJson(ref reader, options);
            CostToBeneficiary.Add(objCostToBeneficiary);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (CostToBeneficiary.Count == 0)
          {
            CostToBeneficiary = null;
          }

          break;

        case "dependent":
          Dependent = reader.GetString();
          break;

        case "_dependent":
          _Dependent = new fhirCsR4.Models.Element();
          _Dependent.DeserializeJson(ref reader, options);
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

        case "network":
          Network = reader.GetString();
          break;

        case "_network":
          _Network = new fhirCsR4.Models.Element();
          _Network.DeserializeJson(ref reader, options);
          break;

        case "order":
          Order = reader.GetUInt32();
          break;

        case "payor":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Payor = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Reference objPayor = new fhirCsR4.Models.Reference();
            objPayor.DeserializeJson(ref reader, options);
            Payor.Add(objPayor);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Payor.Count == 0)
          {
            Payor = null;
          }

          break;

        case "period":
          Period = new fhirCsR4.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "policyHolder":
          PolicyHolder = new fhirCsR4.Models.Reference();
          PolicyHolder.DeserializeJson(ref reader, options);
          break;

        case "relationship":
          Relationship = new fhirCsR4.Models.CodeableConcept();
          Relationship.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR4.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subrogation":
          Subrogation = reader.GetBoolean();
          break;

        case "subscriber":
          Subscriber = new fhirCsR4.Models.Reference();
          Subscriber.DeserializeJson(ref reader, options);
          break;

        case "subscriberId":
          SubscriberId = reader.GetString();
          break;

        case "_subscriberId":
          _SubscriberId = new fhirCsR4.Models.Element();
          _SubscriberId.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR4.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
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
  /// Code Values for the Coverage.status field
  /// </summary>
  public static class CoverageStatusCodes {
    public const string ACTIVE = "active";
    public const string CANCELLED = "cancelled";
    public const string DRAFT = "draft";
    public const string ENTERED_IN_ERROR = "entered-in-error";
  }
}
