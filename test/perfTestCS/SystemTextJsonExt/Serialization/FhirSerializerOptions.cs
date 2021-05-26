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

namespace Hl7.Fhir.Serialization
{
  /// <summary>
  /// Default JsonSerializerOptions to format JSON serialization as expected.
  /// </summary>
  public static class FhirSerializerOptions
  {
#pragma warning disable CA1810 // Initialize reference type static fields inline

    /// <summary>
    /// Serializer options for Converters (polymorphic deserialization).
    /// </summary>
    private static readonly JsonSerializerOptions _serializerOptions;

    /// <summary>
    /// Compact format internal variable.
    /// </summary>
    private static readonly JsonWriterOptions _compactFormat;

    /// <summary>
    /// Pretty print format internal variable.
    /// </summary>
    private static readonly JsonWriterOptions _prettyFormat;

    /// <summary>
    /// Initializes static members of the &lt;see cref="FhirSerializerOptions"/&gt; class.
    /// </summary>
    static FhirSerializerOptions()
    {
      _serializerOptions = new JsonSerializerOptions()
      {
        Converters =
        {
          new AddressJsonExtensions.AddressJsonConverter(),
          new AgeJsonExtensions.AgeJsonConverter(),
          new AnnotationJsonExtensions.AnnotationJsonConverter(),
          new AttachmentJsonExtensions.AttachmentJsonConverter(),
          new CodeableConceptJsonExtensions.CodeableConceptJsonConverter(),
          new CodingJsonExtensions.CodingJsonConverter(),
          new ContactDetailJsonExtensions.ContactDetailJsonConverter(),
          new ContactPointJsonExtensions.ContactPointJsonConverter(),
          new ContributorJsonExtensions.ContributorJsonConverter(),
          new CountJsonExtensions.CountJsonConverter(),
          new DataRequirementJsonExtensions.DataRequirementJsonConverter(),
          new DistanceJsonExtensions.DistanceJsonConverter(),
          new DosageJsonExtensions.DosageJsonConverter(),
          new DurationJsonExtensions.DurationJsonConverter(),
          new ElementDefinitionJsonExtensions.ElementDefinitionJsonConverter(),
          new ExpressionJsonExtensions.ExpressionJsonConverter(),
          new ExtensionJsonExtensions.ExtensionJsonConverter(),
          new HumanNameJsonExtensions.HumanNameJsonConverter(),
          new IdentifierJsonExtensions.IdentifierJsonConverter(),
          new MarketingStatusJsonExtensions.MarketingStatusJsonConverter(),
          new MetaJsonExtensions.MetaJsonConverter(),
          new MoneyJsonExtensions.MoneyJsonConverter(),
          new NarrativeJsonExtensions.NarrativeJsonConverter(),
          new ParameterDefinitionJsonExtensions.ParameterDefinitionJsonConverter(),
          new PeriodJsonExtensions.PeriodJsonConverter(),
          new PopulationJsonExtensions.PopulationJsonConverter(),
          new ProdCharacteristicJsonExtensions.ProdCharacteristicJsonConverter(),
          new ProductShelfLifeJsonExtensions.ProductShelfLifeJsonConverter(),
          new QuantityJsonExtensions.QuantityJsonConverter(),
          new RangeJsonExtensions.RangeJsonConverter(),
          new RatioJsonExtensions.RatioJsonConverter(),
          new ResourceReferenceJsonExtensions.ResourceReferenceJsonConverter(),
          new RelatedArtifactJsonExtensions.RelatedArtifactJsonConverter(),
          new SampledDataJsonExtensions.SampledDataJsonConverter(),
          new SignatureJsonExtensions.SignatureJsonConverter(),
          new SubstanceAmountJsonExtensions.SubstanceAmountJsonConverter(),
          new TimingJsonExtensions.TimingJsonConverter(),
          new TriggerDefinitionJsonExtensions.TriggerDefinitionJsonConverter(),
          new UsageContextJsonExtensions.UsageContextJsonConverter(),
          new AccountJsonExtensions.AccountJsonConverter(),
          new ActivityDefinitionJsonExtensions.ActivityDefinitionJsonConverter(),
          new AdverseEventJsonExtensions.AdverseEventJsonConverter(),
          new AllergyIntoleranceJsonExtensions.AllergyIntoleranceJsonConverter(),
          new AppointmentJsonExtensions.AppointmentJsonConverter(),
          new AppointmentResponseJsonExtensions.AppointmentResponseJsonConverter(),
          new AuditEventJsonExtensions.AuditEventJsonConverter(),
          new BasicJsonExtensions.BasicJsonConverter(),
          new BinaryJsonExtensions.BinaryJsonConverter(),
          new BiologicallyDerivedProductJsonExtensions.BiologicallyDerivedProductJsonConverter(),
          new BodyStructureJsonExtensions.BodyStructureJsonConverter(),
          new BundleJsonExtensions.BundleJsonConverter(),
          new CapabilityStatementJsonExtensions.CapabilityStatementJsonConverter(),
          new CarePlanJsonExtensions.CarePlanJsonConverter(),
          new CareTeamJsonExtensions.CareTeamJsonConverter(),
          new CatalogEntryJsonExtensions.CatalogEntryJsonConverter(),
          new ChargeItemJsonExtensions.ChargeItemJsonConverter(),
          new ChargeItemDefinitionJsonExtensions.ChargeItemDefinitionJsonConverter(),
          new ClaimJsonExtensions.ClaimJsonConverter(),
          new ClaimResponseJsonExtensions.ClaimResponseJsonConverter(),
          new ClinicalImpressionJsonExtensions.ClinicalImpressionJsonConverter(),
          new CodeSystemJsonExtensions.CodeSystemJsonConverter(),
          new CommunicationJsonExtensions.CommunicationJsonConverter(),
          new CommunicationRequestJsonExtensions.CommunicationRequestJsonConverter(),
          new CompartmentDefinitionJsonExtensions.CompartmentDefinitionJsonConverter(),
          new CompositionJsonExtensions.CompositionJsonConverter(),
          new ConceptMapJsonExtensions.ConceptMapJsonConverter(),
          new ConditionJsonExtensions.ConditionJsonConverter(),
          new ConsentJsonExtensions.ConsentJsonConverter(),
          new ContractJsonExtensions.ContractJsonConverter(),
          new CoverageJsonExtensions.CoverageJsonConverter(),
          new CoverageEligibilityRequestJsonExtensions.CoverageEligibilityRequestJsonConverter(),
          new CoverageEligibilityResponseJsonExtensions.CoverageEligibilityResponseJsonConverter(),
          new DetectedIssueJsonExtensions.DetectedIssueJsonConverter(),
          new DeviceJsonExtensions.DeviceJsonConverter(),
          new DeviceDefinitionJsonExtensions.DeviceDefinitionJsonConverter(),
          new DeviceMetricJsonExtensions.DeviceMetricJsonConverter(),
          new DeviceRequestJsonExtensions.DeviceRequestJsonConverter(),
          new DeviceUseStatementJsonExtensions.DeviceUseStatementJsonConverter(),
          new DiagnosticReportJsonExtensions.DiagnosticReportJsonConverter(),
          new DocumentManifestJsonExtensions.DocumentManifestJsonConverter(),
          new DocumentReferenceJsonExtensions.DocumentReferenceJsonConverter(),
          new EffectEvidenceSynthesisJsonExtensions.EffectEvidenceSynthesisJsonConverter(),
          new EncounterJsonExtensions.EncounterJsonConverter(),
          new EndpointJsonExtensions.EndpointJsonConverter(),
          new EnrollmentRequestJsonExtensions.EnrollmentRequestJsonConverter(),
          new EnrollmentResponseJsonExtensions.EnrollmentResponseJsonConverter(),
          new EpisodeOfCareJsonExtensions.EpisodeOfCareJsonConverter(),
          new EventDefinitionJsonExtensions.EventDefinitionJsonConverter(),
          new EvidenceJsonExtensions.EvidenceJsonConverter(),
          new EvidenceVariableJsonExtensions.EvidenceVariableJsonConverter(),
          new ExampleScenarioJsonExtensions.ExampleScenarioJsonConverter(),
          new ExplanationOfBenefitJsonExtensions.ExplanationOfBenefitJsonConverter(),
          new FamilyMemberHistoryJsonExtensions.FamilyMemberHistoryJsonConverter(),
          new FlagJsonExtensions.FlagJsonConverter(),
          new GoalJsonExtensions.GoalJsonConverter(),
          new GraphDefinitionJsonExtensions.GraphDefinitionJsonConverter(),
          new GroupJsonExtensions.GroupJsonConverter(),
          new GuidanceResponseJsonExtensions.GuidanceResponseJsonConverter(),
          new HealthcareServiceJsonExtensions.HealthcareServiceJsonConverter(),
          new ImagingStudyJsonExtensions.ImagingStudyJsonConverter(),
          new ImmunizationJsonExtensions.ImmunizationJsonConverter(),
          new ImmunizationEvaluationJsonExtensions.ImmunizationEvaluationJsonConverter(),
          new ImmunizationRecommendationJsonExtensions.ImmunizationRecommendationJsonConverter(),
          new ImplementationGuideJsonExtensions.ImplementationGuideJsonConverter(),
          new InsurancePlanJsonExtensions.InsurancePlanJsonConverter(),
          new InvoiceJsonExtensions.InvoiceJsonConverter(),
          new LibraryJsonExtensions.LibraryJsonConverter(),
          new LinkageJsonExtensions.LinkageJsonConverter(),
          new ListJsonExtensions.ListJsonConverter(),
          new LocationJsonExtensions.LocationJsonConverter(),
          new MeasureJsonExtensions.MeasureJsonConverter(),
          new MeasureReportJsonExtensions.MeasureReportJsonConverter(),
          new MediaJsonExtensions.MediaJsonConverter(),
          new MedicationJsonExtensions.MedicationJsonConverter(),
          new MedicationAdministrationJsonExtensions.MedicationAdministrationJsonConverter(),
          new MedicationDispenseJsonExtensions.MedicationDispenseJsonConverter(),
          new MedicationKnowledgeJsonExtensions.MedicationKnowledgeJsonConverter(),
          new MedicationRequestJsonExtensions.MedicationRequestJsonConverter(),
          new MedicationStatementJsonExtensions.MedicationStatementJsonConverter(),
          new MedicinalProductJsonExtensions.MedicinalProductJsonConverter(),
          new MedicinalProductAuthorizationJsonExtensions.MedicinalProductAuthorizationJsonConverter(),
          new MedicinalProductContraindicationJsonExtensions.MedicinalProductContraindicationJsonConverter(),
          new MedicinalProductIndicationJsonExtensions.MedicinalProductIndicationJsonConverter(),
          new MedicinalProductIngredientJsonExtensions.MedicinalProductIngredientJsonConverter(),
          new MedicinalProductInteractionJsonExtensions.MedicinalProductInteractionJsonConverter(),
          new MedicinalProductManufacturedJsonExtensions.MedicinalProductManufacturedJsonConverter(),
          new MedicinalProductPackagedJsonExtensions.MedicinalProductPackagedJsonConverter(),
          new MedicinalProductPharmaceuticalJsonExtensions.MedicinalProductPharmaceuticalJsonConverter(),
          new MedicinalProductUndesirableEffectJsonExtensions.MedicinalProductUndesirableEffectJsonConverter(),
          new MessageDefinitionJsonExtensions.MessageDefinitionJsonConverter(),
          new MessageHeaderJsonExtensions.MessageHeaderJsonConverter(),
          new MolecularSequenceJsonExtensions.MolecularSequenceJsonConverter(),
          new NamingSystemJsonExtensions.NamingSystemJsonConverter(),
          new NutritionOrderJsonExtensions.NutritionOrderJsonConverter(),
          new ObservationJsonExtensions.ObservationJsonConverter(),
          new ObservationDefinitionJsonExtensions.ObservationDefinitionJsonConverter(),
          new OperationDefinitionJsonExtensions.OperationDefinitionJsonConverter(),
          new OperationOutcomeJsonExtensions.OperationOutcomeJsonConverter(),
          new OrganizationJsonExtensions.OrganizationJsonConverter(),
          new OrganizationAffiliationJsonExtensions.OrganizationAffiliationJsonConverter(),
          new ParametersJsonExtensions.ParametersJsonConverter(),
          new PatientJsonExtensions.PatientJsonConverter(),
          new PaymentNoticeJsonExtensions.PaymentNoticeJsonConverter(),
          new PaymentReconciliationJsonExtensions.PaymentReconciliationJsonConverter(),
          new PersonJsonExtensions.PersonJsonConverter(),
          new PlanDefinitionJsonExtensions.PlanDefinitionJsonConverter(),
          new PractitionerJsonExtensions.PractitionerJsonConverter(),
          new PractitionerRoleJsonExtensions.PractitionerRoleJsonConverter(),
          new ProcedureJsonExtensions.ProcedureJsonConverter(),
          new ProvenanceJsonExtensions.ProvenanceJsonConverter(),
          new QuestionnaireJsonExtensions.QuestionnaireJsonConverter(),
          new QuestionnaireResponseJsonExtensions.QuestionnaireResponseJsonConverter(),
          new RelatedPersonJsonExtensions.RelatedPersonJsonConverter(),
          new RequestGroupJsonExtensions.RequestGroupJsonConverter(),
          new ResearchDefinitionJsonExtensions.ResearchDefinitionJsonConverter(),
          new ResearchElementDefinitionJsonExtensions.ResearchElementDefinitionJsonConverter(),
          new ResearchStudyJsonExtensions.ResearchStudyJsonConverter(),
          new ResearchSubjectJsonExtensions.ResearchSubjectJsonConverter(),
          new RiskAssessmentJsonExtensions.RiskAssessmentJsonConverter(),
          new RiskEvidenceSynthesisJsonExtensions.RiskEvidenceSynthesisJsonConverter(),
          new ScheduleJsonExtensions.ScheduleJsonConverter(),
          new SearchParameterJsonExtensions.SearchParameterJsonConverter(),
          new ServiceRequestJsonExtensions.ServiceRequestJsonConverter(),
          new SlotJsonExtensions.SlotJsonConverter(),
          new SpecimenJsonExtensions.SpecimenJsonConverter(),
          new SpecimenDefinitionJsonExtensions.SpecimenDefinitionJsonConverter(),
          new StructureDefinitionJsonExtensions.StructureDefinitionJsonConverter(),
          new StructureMapJsonExtensions.StructureMapJsonConverter(),
          new SubscriptionJsonExtensions.SubscriptionJsonConverter(),
          new SubstanceJsonExtensions.SubstanceJsonConverter(),
          new SubstanceNucleicAcidJsonExtensions.SubstanceNucleicAcidJsonConverter(),
          new SubstancePolymerJsonExtensions.SubstancePolymerJsonConverter(),
          new SubstanceProteinJsonExtensions.SubstanceProteinJsonConverter(),
          new SubstanceReferenceInformationJsonExtensions.SubstanceReferenceInformationJsonConverter(),
          new SubstanceSourceMaterialJsonExtensions.SubstanceSourceMaterialJsonConverter(),
          new SubstanceSpecificationJsonExtensions.SubstanceSpecificationJsonConverter(),
          new SupplyDeliveryJsonExtensions.SupplyDeliveryJsonConverter(),
          new SupplyRequestJsonExtensions.SupplyRequestJsonConverter(),
          new TaskJsonExtensions.TaskJsonConverter(),
          new TerminologyCapabilitiesJsonExtensions.TerminologyCapabilitiesJsonConverter(),
          new TestReportJsonExtensions.TestReportJsonConverter(),
          new TestScriptJsonExtensions.TestScriptJsonConverter(),
          new ValueSetJsonExtensions.ValueSetJsonConverter(),
          new VerificationResultJsonExtensions.VerificationResultJsonConverter(),
          new VisionPrescriptionJsonExtensions.VisionPrescriptionJsonConverter(),
        },
      };

      _compactFormat = new JsonWriterOptions()
      {
        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
        Indented = false,
      };

      _prettyFormat = new JsonWriterOptions()
      {
        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
        Indented = true,
      };

    }
#pragma warning restore CA1810 // Initialize reference type static fields inline

    /// <summary>
    /// Serializer options for Converters (polymorphic deserialization).
    /// </summary>
    public static JsonSerializerOptions SerializerOptions => _serializerOptions;

    /// <summary>
    /// Compact (no extra whitespace) format.
    /// </summary>
    public static JsonWriterOptions Compact => _compactFormat;

    /// <summary>
    /// Pretty-printed (newlines and indentation) format.
    /// </summary>
    public static JsonWriterOptions Pretty => _prettyFormat;
  }
}
// end of file
