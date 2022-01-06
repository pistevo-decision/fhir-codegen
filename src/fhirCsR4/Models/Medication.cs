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
  /// The ingredients need not be a complete list.  If an ingredient is not specified, this does not indicate whether an ingredient is present or absent.  If an ingredient is specified it does not mean that all ingredients are specified.  It is possible to specify both inactive and active ingredients.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<MedicationIngredient>))]
  public class MedicationIngredient : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Indication of whether this ingredient affects the therapeutic action of the drug.
    /// </summary>
    public bool? IsActive { get; set; }
    /// <summary>
    /// The actual ingredient - either a substance (simple ingredient) or another medication of a medication.
    /// </summary>
    public CodeableConcept ItemCodeableConcept { get; set; }
    /// <summary>
    /// The actual ingredient - either a substance (simple ingredient) or another medication of a medication.
    /// </summary>
    public Reference ItemReference { get; set; }
    /// <summary>
    /// Specifies how many (or how much) of the items there are in this Medication.  For example, 250 mg per tablet.  This is expressed as a ratio where the numerator is 250mg and the denominator is 1 tablet.
    /// </summary>
    public Ratio Strength { get; set; }
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

      if (ItemCodeableConcept != null)
      {
        writer.WritePropertyName("itemCodeableConcept");
        ItemCodeableConcept.SerializeJson(writer, options);
      }

      if (ItemReference != null)
      {
        writer.WritePropertyName("itemReference");
        ItemReference.SerializeJson(writer, options);
      }

      if (IsActive != null)
      {
        writer.WriteBoolean("isActive", (bool)IsActive!);
      }

      if (Strength != null)
      {
        writer.WritePropertyName("strength");
        Strength.SerializeJson(writer, options);
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
        case "isActive":
          IsActive = reader.GetBoolean();
          break;

        case "itemCodeableConcept":
          ItemCodeableConcept = new fhirCsR4.Models.CodeableConcept();
          ItemCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "itemReference":
          ItemReference = new fhirCsR4.Models.Reference();
          ItemReference.DeserializeJson(ref reader, options);
          break;

        case "strength":
          Strength = new fhirCsR4.Models.Ratio();
          Strength.DeserializeJson(ref reader, options);
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
  /// Information that only applies to packages (not products).
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<MedicationBatch>))]
  public class MedicationBatch : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// When this specific batch of product will expire.
    /// </summary>
    public string ExpirationDate { get; set; }
    /// <summary>
    /// Extension container element for ExpirationDate
    /// </summary>
    public Element _ExpirationDate { get; set; }
    /// <summary>
    /// The assigned lot number of a batch of the specified product.
    /// </summary>
    public string LotNumber { get; set; }
    /// <summary>
    /// Extension container element for LotNumber
    /// </summary>
    public Element _LotNumber { get; set; }
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

      if (!string.IsNullOrEmpty(LotNumber))
      {
        writer.WriteString("lotNumber", (string)LotNumber!);
      }

      if (_LotNumber != null)
      {
        writer.WritePropertyName("_lotNumber");
        _LotNumber.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ExpirationDate))
      {
        writer.WriteString("expirationDate", (string)ExpirationDate!);
      }

      if (_ExpirationDate != null)
      {
        writer.WritePropertyName("_expirationDate");
        _ExpirationDate.SerializeJson(writer, options);
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
        case "expirationDate":
          ExpirationDate = reader.GetString();
          break;

        case "_expirationDate":
          _ExpirationDate = new fhirCsR4.Models.Element();
          _ExpirationDate.DeserializeJson(ref reader, options);
          break;

        case "lotNumber":
          LotNumber = reader.GetString();
          break;

        case "_lotNumber":
          _LotNumber = new fhirCsR4.Models.Element();
          _LotNumber.DeserializeJson(ref reader, options);
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
  /// This resource is primarily used for the identification and definition of a medication for the purposes of prescribing, dispensing, and administering a medication as well as for making statements about medication use.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<Medication>))]
  public class Medication : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Medication";
    /// <summary>
    /// Specific amount of the drug in the packaged product.  For example, when specifying a product that has the same strength (For example, Insulin glargine 100 unit per mL solution for injection), this attribute provides additional clarification of the package amount (For example, 3 mL, 10mL, etc.).
    /// </summary>
    public Ratio Amount { get; set; }
    /// <summary>
    /// Information that only applies to packages (not products).
    /// </summary>
    public MedicationBatch Batch { get; set; }
    /// <summary>
    /// Depending on the context of use, the code that was actually selected by the user (prescriber, dispenser, etc.) will have the coding.userSelected set to true.  As described in the coding datatype: "A coding may be marked as a "userSelected" if a user selected the particular coded value in a user interface (e.g. the user selects an item in a pick-list). If a user selected coding exists, it is the preferred choice for performing translations etc. Other codes can only be literal translations to alternative code systems, or codes at a lower level of granularity (e.g. a generic code for a vendor-specific primary one).
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// When Medication is referenced from MedicationRequest, this is the ordered form.  When Medication is referenced within MedicationDispense, this is the dispensed form.  When Medication is referenced within MedicationAdministration, this is administered form.
    /// </summary>
    public CodeableConcept Form { get; set; }
    /// <summary>
    /// The serial number could be included as an identifier.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The ingredients need not be a complete list.  If an ingredient is not specified, this does not indicate whether an ingredient is present or absent.  If an ingredient is specified it does not mean that all ingredients are specified.  It is possible to specify both inactive and active ingredients.
    /// </summary>
    public List<MedicationIngredient> Ingredient { get; set; }
    /// <summary>
    /// Describes the details of the manufacturer of the medication product.  This is not intended to represent the distributor of a medication product.
    /// </summary>
    public Reference Manufacturer { get; set; }
    /// <summary>
    /// This status is intended to identify if the medication in a local system is in active use within a drug database or inventory.  For example, a pharmacy system may create a new drug file record for a compounded product "ABC Hospital Special Cream" with an active status.  At some point in the future, it may be determined that the drug record was created with an error and the status is changed to "entered in error".   This status is not intended to specify if a medication is part of a particular formulary.  It is possible that the drug record may be referenced by multiple formularies or catalogues and each of those entries would have a separate status.
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

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
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

      if (Manufacturer != null)
      {
        writer.WritePropertyName("manufacturer");
        Manufacturer.SerializeJson(writer, options);
      }

      if (Form != null)
      {
        writer.WritePropertyName("form");
        Form.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
      }

      if ((Ingredient != null) && (Ingredient.Count != 0))
      {
        writer.WritePropertyName("ingredient");
        writer.WriteStartArray();

        foreach (MedicationIngredient valIngredient in Ingredient)
        {
          valIngredient.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Batch != null)
      {
        writer.WritePropertyName("batch");
        Batch.SerializeJson(writer, options);
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
        case "amount":
          Amount = new fhirCsR4.Models.Ratio();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "batch":
          Batch = new fhirCsR4.Models.MedicationBatch();
          Batch.DeserializeJson(ref reader, options);
          break;

        case "code":
          Code = new fhirCsR4.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "form":
          Form = new fhirCsR4.Models.CodeableConcept();
          Form.DeserializeJson(ref reader, options);
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

        case "ingredient":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Ingredient = new List<MedicationIngredient>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.MedicationIngredient objIngredient = new fhirCsR4.Models.MedicationIngredient();
            objIngredient.DeserializeJson(ref reader, options);
            Ingredient.Add(objIngredient);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Ingredient.Count == 0)
          {
            Ingredient = null;
          }

          break;

        case "manufacturer":
          Manufacturer = new fhirCsR4.Models.Reference();
          Manufacturer.DeserializeJson(ref reader, options);
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
  /// Code Values for the Medication.status field
  /// </summary>
  public static class MedicationStatusCodes {
    public const string ACTIVE = "active";
    public const string INACTIVE = "inactive";
    public const string ENTERED_IN_ERROR = "entered-in-error";
  }
}
