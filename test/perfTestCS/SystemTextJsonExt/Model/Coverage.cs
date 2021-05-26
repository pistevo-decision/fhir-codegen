// <auto-generated/>
// Contents of: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Hl7.Fhir.Model;
using Hl7.Fhir.Model.JsonExtensions;
using Hl7.Fhir.Serialization;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model.JsonExtensions
{
  /// <summary>
  /// JSON Serialization Extensions for Coverage
  /// </summary>
  public static class CoverageJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR Coverage into JSON
    /// </summary>
    public static void SerializeJson(this Coverage current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","Coverage");
      // Complex: Coverage, Export: Coverage, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if ((current.Identifier != null) && (current.Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();
        foreach (Identifier val in current.Identifier)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      if (current.Type != null)
      {
        writer.WritePropertyName("type");
        current.Type.SerializeJson(writer, options);
      }

      if (current.PolicyHolder != null)
      {
        writer.WritePropertyName("policyHolder");
        current.PolicyHolder.SerializeJson(writer, options);
      }

      if (current.Subscriber != null)
      {
        writer.WritePropertyName("subscriber");
        current.Subscriber.SerializeJson(writer, options);
      }

      if (current.SubscriberIdElement != null)
      {
        if (!string.IsNullOrEmpty(current.SubscriberIdElement.Value))
        {
          writer.WriteString("subscriberId",current.SubscriberIdElement.Value);
        }
        if (current.SubscriberIdElement.HasExtensions() || (!string.IsNullOrEmpty(current.SubscriberIdElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_subscriberId",false,current.SubscriberIdElement.Extension,current.SubscriberIdElement.ElementId);
        }
      }

      writer.WritePropertyName("beneficiary");
      current.Beneficiary.SerializeJson(writer, options);

      if (current.DependentElement != null)
      {
        if (!string.IsNullOrEmpty(current.DependentElement.Value))
        {
          writer.WriteString("dependent",current.DependentElement.Value);
        }
        if (current.DependentElement.HasExtensions() || (!string.IsNullOrEmpty(current.DependentElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_dependent",false,current.DependentElement.Extension,current.DependentElement.ElementId);
        }
      }

      if (current.Relationship != null)
      {
        writer.WritePropertyName("relationship");
        current.Relationship.SerializeJson(writer, options);
      }

      if (current.Period != null)
      {
        writer.WritePropertyName("period");
        current.Period.SerializeJson(writer, options);
      }

      if ((current.Payor != null) && (current.Payor.Count != 0))
      {
        writer.WritePropertyName("payor");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Payor)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Class != null) && (current.Class.Count != 0))
      {
        writer.WritePropertyName("class");
        writer.WriteStartArray();
        foreach (Coverage.ClassComponent val in current.Class)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.OrderElement != null)
      {
        if (current.OrderElement.Value != null)
        {
          writer.WriteNumber("order",(int)current.OrderElement.Value);
        }
        if (current.OrderElement.HasExtensions() || (!string.IsNullOrEmpty(current.OrderElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_order",false,current.OrderElement.Extension,current.OrderElement.ElementId);
        }
      }

      if (current.NetworkElement != null)
      {
        if (!string.IsNullOrEmpty(current.NetworkElement.Value))
        {
          writer.WriteString("network",current.NetworkElement.Value);
        }
        if (current.NetworkElement.HasExtensions() || (!string.IsNullOrEmpty(current.NetworkElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_network",false,current.NetworkElement.Extension,current.NetworkElement.ElementId);
        }
      }

      if ((current.CostToBeneficiary != null) && (current.CostToBeneficiary.Count != 0))
      {
        writer.WritePropertyName("costToBeneficiary");
        writer.WriteStartArray();
        foreach (Coverage.CostToBeneficiaryComponent val in current.CostToBeneficiary)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.SubrogationElement != null)
      {
        if (current.SubrogationElement.Value != null)
        {
          writer.WriteBoolean("subrogation",(bool)current.SubrogationElement.Value);
        }
        if (current.SubrogationElement.HasExtensions() || (!string.IsNullOrEmpty(current.SubrogationElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_subrogation",false,current.SubrogationElement.Extension,current.SubrogationElement.ElementId);
        }
      }

      if ((current.Contract != null) && (current.Contract.Count != 0))
      {
        writer.WritePropertyName("contract");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Contract)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage
    /// </summary>
    public static void DeserializeJson(this Coverage current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage
    /// </summary>
    public static void DeserializeJsonProperty(this Coverage current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "status":
          current.StatusElement =new Code<Hl7.Fhir.Model.FinancialResourceStatusCodes>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.FinancialResourceStatusCodes>(reader.GetString()));
          break;

        case "_status":
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "policyHolder":
          current.PolicyHolder = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.PolicyHolder).DeserializeJson(ref reader, options);
          break;

        case "subscriber":
          current.Subscriber = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Subscriber).DeserializeJson(ref reader, options);
          break;

        case "subscriberId":
          current.SubscriberIdElement = new FhirString(reader.GetString());
          break;

        case "_subscriberId":
          ((Hl7.Fhir.Model.Element)current.SubscriberIdElement).DeserializeJson(ref reader, options);
          break;

        case "beneficiary":
          current.Beneficiary = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Beneficiary).DeserializeJson(ref reader, options);
          break;

        case "dependent":
          current.DependentElement = new FhirString(reader.GetString());
          break;

        case "_dependent":
          ((Hl7.Fhir.Model.Element)current.DependentElement).DeserializeJson(ref reader, options);
          break;

        case "relationship":
          current.Relationship = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Relationship).DeserializeJson(ref reader, options);
          break;

        case "period":
          current.Period = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Period).DeserializeJson(ref reader, options);
          break;

        case "payor":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Payor = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Payor = new Hl7.Fhir.Model.ResourceReference();
            v_Payor.DeserializeJson(ref reader, options);
            current.Payor.Add(v_Payor);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Payor.Count == 0)
          {
            current.Payor = null;
          }
          break;

        case "class":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Class = new List<Coverage.ClassComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Coverage.ClassComponent v_Class = new Hl7.Fhir.Model.Coverage.ClassComponent();
            v_Class.DeserializeJson(ref reader, options);
            current.Class.Add(v_Class);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Class.Count == 0)
          {
            current.Class = null;
          }
          break;

        case "order":
          current.OrderElement = new PositiveInt(reader.GetInt32());
          break;

        case "_order":
          ((Hl7.Fhir.Model.Element)current.OrderElement).DeserializeJson(ref reader, options);
          break;

        case "network":
          current.NetworkElement = new FhirString(reader.GetString());
          break;

        case "_network":
          ((Hl7.Fhir.Model.Element)current.NetworkElement).DeserializeJson(ref reader, options);
          break;

        case "costToBeneficiary":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.CostToBeneficiary = new List<Coverage.CostToBeneficiaryComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Coverage.CostToBeneficiaryComponent v_CostToBeneficiary = new Hl7.Fhir.Model.Coverage.CostToBeneficiaryComponent();
            v_CostToBeneficiary.DeserializeJson(ref reader, options);
            current.CostToBeneficiary.Add(v_CostToBeneficiary);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.CostToBeneficiary.Count == 0)
          {
            current.CostToBeneficiary = null;
          }
          break;

        case "subrogation":
          current.SubrogationElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "_subrogation":
          ((Hl7.Fhir.Model.Element)current.SubrogationElement).DeserializeJson(ref reader, options);
          break;

        case "contract":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Contract = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Contract = new Hl7.Fhir.Model.ResourceReference();
            v_Contract.DeserializeJson(ref reader, options);
            current.Contract.Add(v_Contract);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Contract.Count == 0)
          {
            current.Contract = null;
          }
          break;

        // Complex: Coverage, Export: Coverage, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Coverage#Class into JSON
    /// </summary>
    public static void SerializeJson(this Coverage.ClassComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Coverage#Class, Export: ClassComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WritePropertyName("type");
      current.Type.SerializeJson(writer, options);

      writer.WriteString("value",current.ValueElement.Value);

      if (current.NameElement != null)
      {
        if (!string.IsNullOrEmpty(current.NameElement.Value))
        {
          writer.WriteString("name",current.NameElement.Value);
        }
        if (current.NameElement.HasExtensions() || (!string.IsNullOrEmpty(current.NameElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_name",false,current.NameElement.Extension,current.NameElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage#Class
    /// </summary>
    public static void DeserializeJson(this Coverage.ClassComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage#Class
    /// </summary>
    public static void DeserializeJsonProperty(this Coverage.ClassComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "value":
          current.ValueElement = new FhirString(reader.GetString());
          break;

        case "_value":
          ((Hl7.Fhir.Model.Element)current.ValueElement).DeserializeJson(ref reader, options);
          break;

        case "name":
          current.NameElement = new FhirString(reader.GetString());
          break;

        case "_name":
          ((Hl7.Fhir.Model.Element)current.NameElement).DeserializeJson(ref reader, options);
          break;

        // Complex: class, Export: ClassComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Coverage#CostToBeneficiary into JSON
    /// </summary>
    public static void SerializeJson(this Coverage.CostToBeneficiaryComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Coverage#CostToBeneficiary, Export: CostToBeneficiaryComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Type != null)
      {
        writer.WritePropertyName("type");
        current.Type.SerializeJson(writer, options);
      }

      if (current.Value != null)
      {
        switch (current.Value)
        {
          case Quantity v_Quantity:
            writer.WritePropertyName("valueQuantity");
            v_Quantity.SerializeJson(writer, options);
            break;
          case Money v_Money:
            writer.WritePropertyName("valueMoney");
            v_Money.SerializeJson(writer, options);
            break;
        }
      }
      if ((current.Exception != null) && (current.Exception.Count != 0))
      {
        writer.WritePropertyName("exception");
        writer.WriteStartArray();
        foreach (Coverage.ExemptionComponent val in current.Exception)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage#CostToBeneficiary
    /// </summary>
    public static void DeserializeJson(this Coverage.CostToBeneficiaryComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage#CostToBeneficiary
    /// </summary>
    public static void DeserializeJsonProperty(this Coverage.CostToBeneficiaryComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "valueQuantity":
          current.Value = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.Value).DeserializeJson(ref reader, options);
          break;

        case "valueMoney":
          current.Value = new Hl7.Fhir.Model.Money();
          ((Hl7.Fhir.Model.Money)current.Value).DeserializeJson(ref reader, options);
          break;

        case "exception":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Exception = new List<Coverage.ExemptionComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Coverage.ExemptionComponent v_Exception = new Hl7.Fhir.Model.Coverage.ExemptionComponent();
            v_Exception.DeserializeJson(ref reader, options);
            current.Exception.Add(v_Exception);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Exception.Count == 0)
          {
            current.Exception = null;
          }
          break;

        // Complex: costToBeneficiary, Export: CostToBeneficiaryComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Coverage#Exemption into JSON
    /// </summary>
    public static void SerializeJson(this Coverage.ExemptionComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Coverage#Exemption, Export: ExemptionComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WritePropertyName("type");
      current.Type.SerializeJson(writer, options);

      if (current.Period != null)
      {
        writer.WritePropertyName("period");
        current.Period.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage#Exemption
    /// </summary>
    public static void DeserializeJson(this Coverage.ExemptionComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coverage#Exemption
    /// </summary>
    public static void DeserializeJsonProperty(this Coverage.ExemptionComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "period":
          current.Period = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Period).DeserializeJson(ref reader, options);
          break;

        // Complex: exception, Export: ExemptionComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class CoverageJsonConverter : JsonConverter<Coverage>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(Coverage).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, Coverage value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override Coverage Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        Coverage target = new Coverage();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
