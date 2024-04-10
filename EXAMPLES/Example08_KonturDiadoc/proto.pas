unit proto;

{=============================================================================
  This file was generated automatically by the ProtoBufParser for Delphi v.1.0
  Date: 31.03.2024 12:26:49

  Files: 
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\AcquireCounteragent.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentId.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\LockMode.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CustomDataItem.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionRequestType.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionType.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Signer.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRequestInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionAction.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionTarget.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\OrganizationInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Address.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentType.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentDirection.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedOrganizationInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\AsyncMethodResult.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CertificateInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Certificates\CertificateInfoV2.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Certificates\CertificateList.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CloudSign.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Content_v2.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Content.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Content_v3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Counteragent.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CustomPrintFormDetection.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\Department.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Timestamp.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\Routing.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentList.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToCreate.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Attachment.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\SignatureVerificationResult.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\AttachmentV3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\BilateralDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ReceiptDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RecipientSignatureDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RecipientSignatureRejectionDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Docflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\InvoiceDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\UnilateralDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\XmlBilateralDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RevocationDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\UniversalTransferDocumentDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RoamingNotification.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\TotalCountType.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\TimeBasedFilter.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentWithDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ForwardDocumentEvent.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentWithDocflowV3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\FullVersion.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\AcceptanceCertificateDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\ReceiptStatus.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\InvoiceDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\NonformalizedDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UnilateralDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UniversalTransferDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OuterDocflow.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OuterDocflowStatus.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DetectTitleResponse.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentList.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentProtocol.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentsMoveOperation.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentZip.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescription.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Employee.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\User.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUserPermissions.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToCreate.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Subscriptions\Subscription.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\CancellationInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRequestDenialInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRouteInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\RevocationRequestInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ExternalServiceAuthInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardedDocument.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\GetOrganizationsByInnList.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificate552Info.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificateInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Official.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\FnsRegistrationMessageInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceCorrectionRequestInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\RevocationRequestInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\SignatureRejectionInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Torg12Info.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\KeyValueStorage\KeyValueStorage.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\LoginPassword.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationPropertiesToUpdate.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\AutoBlockStatus.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\BlockStatus.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\ManualBlockStatus.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\OrganizationFeatures.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUser.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Recognition\Recognition.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Registration\RegistrationRequest.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionRouteList.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\RoamingNotification.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\SignatureInfo.proto
        C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Users\UserToUpdate.proto

  Available types: 
        Diadoc.Api.Proto.AcquireCounteragentRequest
        Diadoc.Api.Proto.InvitationDocument
        Diadoc.Api.Proto.AcquireCounteragentResult
        Diadoc.Api.Proto.Events.MessageToPost
        Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment
        Diadoc.Api.Proto.Events.EncryptedInvoiceAttachment
        Diadoc.Api.Proto.Events.EncryptedDocumentMetadata
        Diadoc.Api.Proto.Events.EncryptedXmlBasicDocumentMetadata
        Diadoc.Api.Proto.Events.EncryptedInvoiceMetadata
        Diadoc.Api.Proto.Events.EncryptedInvoiceCorrectionMetadata
        Diadoc.Api.Proto.Events.XmlDocumentAttachment
        Diadoc.Api.Proto.Events.NonformalizedAttachment
        Diadoc.Api.Proto.Events.BasicDocumentAttachment
        Diadoc.Api.Proto.Events.Torg13Attachment
        Diadoc.Api.Proto.Events.AcceptanceCertificateAttachment
        Diadoc.Api.Proto.Events.TrustConnectionRequestAttachment
        Diadoc.Api.Proto.Events.StructuredDataAttachment
        Diadoc.Api.Proto.Events.PriceListAttachment
        Diadoc.Api.Proto.Events.ReconciliationActAttachment
        Diadoc.Api.Proto.Events.ContractAttachment
        Diadoc.Api.Proto.Events.SupplementaryAgreementAttachment
        Diadoc.Api.Proto.Events.ServiceDetailsAttachment
        Diadoc.Api.Proto.Events.DocumentAttachment
        Diadoc.Api.Proto.Events.MetadataItem
        Diadoc.Api.Proto.Events.MessagePatchToPost
        Diadoc.Api.Proto.Events.EditingPatch
        Diadoc.Api.Proto.Events.SignatureVerification
        Diadoc.Api.Proto.Events.ResolutionRequestAttachment
        Diadoc.Api.Proto.Events.ResolutionRouteAssignment
        Diadoc.Api.Proto.Events.ResolutionRequestCancellationAttachment
        Diadoc.Api.Proto.Events.ResolutionRequestDenialCancellationAttachment
        Diadoc.Api.Proto.Events.ResolutionRequestDenialAttachment
        Diadoc.Api.Proto.Events.ResolutionAttachment
        Diadoc.Api.Proto.Events.ReceiptAttachment
        Diadoc.Api.Proto.Events.RecipientTitleAttachment
        Diadoc.Api.Proto.Events.CorrectionRequestAttachment
        Diadoc.Api.Proto.Events.DocumentSignature
        Diadoc.Api.Proto.Events.DocumentSenderSignature
        Diadoc.Api.Proto.Events.RequestedSignatureRejection
        Diadoc.Api.Proto.Events.SignedContent
        Diadoc.Api.Proto.Events.DraftToSend
        Diadoc.Api.Proto.Events.PrepareDocumentsToSignRequest
        Diadoc.Api.Proto.Events.DraftDocumentToPatch
        Diadoc.Api.Proto.Events.ContentToPatch
        Diadoc.Api.Proto.Events.DocumentToPatch
        Diadoc.Api.Proto.Events.DocumentPatchedContent
        Diadoc.Api.Proto.Events.PrepareDocumentsToSignResponse
        Diadoc.Api.Proto.Events.MessageToSend
        Diadoc.Api.Proto.Events.RevocationRequestAttachment
        Diadoc.Api.Proto.Events.XmlSignatureRejectionAttachment
        Diadoc.Api.Proto.Events.RoamingNotificationToPost
        Diadoc.Api.Proto.Events.CustomDataPatch
        Diadoc.Api.Proto.Events.EditDocumentPacketCommand
        Diadoc.Api.Proto.Events.ResolutionRouteRemoval
        Diadoc.Api.Proto.Events.TemplateToPost
        Diadoc.Api.Proto.Events.TemplateDocumentAttachment
        Diadoc.Api.Proto.Events.TemplatePatchToPost
        Diadoc.Api.Proto.Events.TemplateRefusalAttachment
        Diadoc.Api.Proto.Events.PredefinedRecipientTitle
        Diadoc.Api.Proto.Events.UnsignedContent
        Diadoc.Api.Proto.Events.TemplateTransformationToPost
        Diadoc.Api.Proto.Events.DocumentTransformation
        Diadoc.Api.Proto.DocumentId
        Diadoc.Api.Proto.DocumentIdEx
        Diadoc.Api.Proto.CustomDataItem
        Diadoc.Api.Proto.Invoicing.Signer
        Diadoc.Api.Proto.Invoicing.SignerDetails
        Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner
        Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetails
        Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetailsToPost
        Diadoc.Api.Proto.Events.ResolutionInfo
        Diadoc.Api.Proto.Events.ResolutionRequestInfo
        Diadoc.Api.Proto.ResolutionTarget
        Diadoc.Api.Proto.Invoicing.DocflowParticipant
        Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo
        Diadoc.Api.Proto.Invoicing.OrganizationInfo
        Diadoc.Api.Proto.Address
        Diadoc.Api.Proto.RussianAddress
        Diadoc.Api.Proto.ForeignAddress
        Diadoc.Api.Proto.Docflow.DocumentInfo
        Diadoc.Api.Proto.Docflow.DocumentDateAndNumber
        Diadoc.Api.Proto.Docflow.BasicDocumentInfo
        Diadoc.Api.Proto.Docflow.InvoiceDocumentInfo
        Diadoc.Api.Proto.Docflow.InvoiceCorrectionDocumentInfo
        Diadoc.Api.Proto.Docflow.PriceListDocumentInfo
        Diadoc.Api.Proto.Docflow.ContractDocumentInfo
        Diadoc.Api.Proto.Docflow.SupplementaryAgreementDocumentInfo
        Diadoc.Api.Proto.Docflow.UniversalTransferDocumentInfo
        Diadoc.Api.Proto.Docflow.UniversalCorrectionDocumentInfo
        Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentSellerTitleInfo
        Diadoc.Api.Proto.Invoicing.Shipper
        Diadoc.Api.Proto.Invoicing.InvoiceTable
        Diadoc.Api.Proto.Invoicing.ExtendedInvoiceItem
        Diadoc.Api.Proto.Invoicing.TransferInfo
        Diadoc.Api.Proto.Invoicing.TransferBase
        Diadoc.Api.Proto.Invoicing.Waybill
        Diadoc.Api.Proto.Invoicing.Employee
        Diadoc.Api.Proto.Invoicing.OtherIssuer
        Diadoc.Api.Proto.Invoicing.AdditionalInfoId
        Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentBuyerTitleInfo
        Diadoc.Api.Proto.Invoicing.UniversalCorrectionDocumentSellerTitleInfo
        Diadoc.Api.Proto.Invoicing.InvoiceForCorrectionInfo
        Diadoc.Api.Proto.Invoicing.InvoiceRevisionInfo
        Diadoc.Api.Proto.Invoicing.EventContent
        Diadoc.Api.Proto.Invoicing.CorrectionBase
        Diadoc.Api.Proto.Invoicing.InvoiceCorrectionTable
        Diadoc.Api.Proto.Invoicing.ExtendedInvoiceCorrectionItem
        Diadoc.Api.Proto.Invoicing.InvoiceInfo
        Diadoc.Api.Proto.Invoicing.AdditionalInfo
        Diadoc.Api.Proto.Invoicing.InvoiceItem
        Diadoc.Api.Proto.Invoicing.CustomsDeclaration
        Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo
        Diadoc.Api.Proto.Invoicing.ShipperOrConsignee
        Diadoc.Api.Proto.Invoicing.InvoiceCorrectionInfo
        Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff
        Diadoc.Api.Proto.Invoicing.InvoiceCorrectionItem
        Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields
        Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff
        Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo
        Diadoc.Api.Proto.AsyncMethodResult
        Diadoc.Api.Proto.CertificateInfo
        Diadoc.Api.Proto.Certificates.CertificateInfoV2
        Diadoc.Api.Proto.Certificates.CertificateList
        Diadoc.Api.Proto.CloudSignRequest
        Diadoc.Api.Proto.CloudSignFile
        Diadoc.Api.Proto.CloudSignResult
        Diadoc.Api.Proto.CloudSignConfirmResult
        Diadoc.Api.Proto.AutosignReceiptsResult
        Diadoc.Api.Proto.Content_v2
        Diadoc.Api.Proto.Content
        Diadoc.Api.Proto.Content_v3
        Diadoc.Api.Proto.CounteragentList
        Diadoc.Api.Proto.Counteragent
        Diadoc.Api.Proto.CounteragentCertificateList
        Diadoc.Api.Proto.Certificate
        Diadoc.Api.Proto.OrganizationList
        Diadoc.Api.Proto.Organization
        Diadoc.Api.Proto.Department
        Diadoc.Api.Proto.Box
        Diadoc.Api.Proto.CustomPrintFormDetectionRequest
        Diadoc.Api.Proto.CustomPrintFormDetectionResult
        Diadoc.Api.Proto.CustomPrintFormDetectionItemResult
        Diadoc.Api.Proto.Departments.Department
        Diadoc.Api.Proto.Timestamp
        Diadoc.Api.Proto.Departments.Routing
        Diadoc.Api.Proto.Departments.DepartmentList
        Diadoc.Api.Proto.Departments.DepartmentToCreate
        Diadoc.Api.Proto.Departments.DepartmentToUpdate
        Diadoc.Api.Proto.Departments.ParentDepartmentPatch
        Diadoc.Api.Proto.Departments.DepartmentNamingPatch
        Diadoc.Api.Proto.Departments.DepartmentKppPatch
        Diadoc.Api.Proto.Departments.DepartmentAddressPatch
        Diadoc.Api.Proto.Departments.DepartmentRoutingPatch
        Diadoc.Api.Proto.Docflow.Entity
        Diadoc.Api.Proto.Docflow.Attachment
        Diadoc.Api.Proto.Docflow.Signature
        Diadoc.Api.Proto.Docflow.SignedAttachment
        Diadoc.Api.Proto.SignatureVerificationResult
        Diadoc.Api.Proto.CertificateVerificationResult
        Diadoc.Api.Proto.CertificateChainElement
        Diadoc.Api.Proto.Docflow.SignatureV3
        Diadoc.Api.Proto.Docflow.SignedAttachmentV3
        Diadoc.Api.Proto.Docflow.BilateralDocflow
        Diadoc.Api.Proto.Docflow.ReceiptDocflow
        Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow
        Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow
        Diadoc.Api.Proto.Docflow.Docflow
        Diadoc.Api.Proto.Docflow.DocflowStatus
        Diadoc.Api.Proto.Docflow.DocflowStatusModel
        Diadoc.Api.Proto.Docflow.InboundInvoiceDocflow
        Diadoc.Api.Proto.Docflow.OutboundInvoiceDocflow
        Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow
        Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow
        Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow
        Diadoc.Api.Proto.Docflow.UnilateralDocflow
        Diadoc.Api.Proto.Docflow.XmlBilateralDocflow
        Diadoc.Api.Proto.Docflow.BuyerTitleDocflow
        Diadoc.Api.Proto.Docflow.RevocationDocflow
        Diadoc.Api.Proto.Docflow.ResolutionDocflow
        Diadoc.Api.Proto.Docflow.InboundUniversalTransferDocumentDocflow
        Diadoc.Api.Proto.Docflow.OutboundUniversalTransferDocumentDocflow
        Diadoc.Api.Proto.Docflow.RoamingNotification
        Diadoc.Api.Proto.Docflow.GetDocflowBatchRequest
        Diadoc.Api.Proto.Docflow.GetDocflowRequest
        Diadoc.Api.Proto.Docflow.GetDocflowBatchResponse
        Diadoc.Api.Proto.Docflow.SearchDocflowsRequest
        Diadoc.Api.Proto.Docflow.SearchDocflowsResponse
        Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdRequest
        Diadoc.Api.Proto.Docflow.FetchedDocument
        Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponse
        Diadoc.Api.Proto.Docflow.GetDocflowEventsRequest
        Diadoc.Api.Proto.Docflow.GetDocflowEventsResponse
        Diadoc.Api.Proto.Docflow.DocflowEvent
        Diadoc.Api.Proto.TimeBasedFilter
        Diadoc.Api.Proto.Docflow.DocumentWithDocflow
        Diadoc.Api.Proto.ForwardDocumentEvent
        Diadoc.Api.Proto.Docflow.GetDocflowBatchResponseV3
        Diadoc.Api.Proto.Docflow.SearchDocflowsResponseV3
        Diadoc.Api.Proto.Docflow.FetchedDocumentV3
        Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponseV3
        Diadoc.Api.Proto.Docflow.GetDocflowEventsResponseV3
        Diadoc.Api.Proto.Docflow.DocflowEventV3
        Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3
        Diadoc.Api.Proto.Docflow.LastEvent
        Diadoc.Api.Proto.Docflow.DocumentInfoV3
        Diadoc.Api.Proto.Docflow.DocumentParticipants
        Diadoc.Api.Proto.Docflow.DocumentParticipant
        Diadoc.Api.Proto.Docflow.DocumentLinks
        Diadoc.Api.Proto.Docflow.PacketInfo
        Diadoc.Api.Proto.Docflow.DocumentLetterInfo
        Diadoc.Api.Proto.Docflow.DocumentDraftInfo
        Diadoc.Api.Proto.Docflow.DocumentTemplateInfo
        Diadoc.Api.Proto.Docflow.TemplateTransformationInfo
        Diadoc.Api.Proto.Docflow.TemplateRefusalInfo
        Diadoc.Api.Proto.FullVersion
        Diadoc.Api.Proto.Documents.Document
        Diadoc.Api.Proto.Documents.LastOuterDocflow
        Diadoc.Api.Proto.Documents.ResolutionStatus
        Diadoc.Api.Proto.Documents.RecipientReceiptMetadata
        Diadoc.Api.Proto.Documents.SenderReceiptMetadata
        Diadoc.Api.Proto.Documents.ConfirmationMetadata
        Diadoc.Api.Proto.Documents.AmendmentRequestMetadata
        Diadoc.Api.Proto.Documents.Origin
        Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateMetadata
        Diadoc.Api.Proto.Documents.BilateralDocument.TrustConnectionRequestMetadata
        Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata
        Diadoc.Api.Proto.Documents.BilateralDocument.PriceListMetadata
        Diadoc.Api.Proto.Documents.BilateralDocument.ContractMetadata
        Diadoc.Api.Proto.Documents.BilateralDocument.SupplementaryAgreementMetadata
        Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentMetadata
        Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceMetadata
        Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceRevisionMetadata
        Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionMetadata
        Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionRevisionMetadata
        Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata
        Diadoc.Api.Proto.Documents.UnilateralDocument.ProformaInvoiceMetadata
        Diadoc.Api.Proto.Documents.UnilateralDocument.ServiceDetailsMetadata
        Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentMetadata
        Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentRevisionMetadata
        Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentMetadata
        Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentRevisionMetadata
        Diadoc.Api.Proto.OuterDocflowInfo
        Diadoc.Api.Proto.Status
        Diadoc.Api.Proto.StatusDetail
        Diadoc.Api.Proto.Docflow.DocflowV3
        Diadoc.Api.Proto.Docflow.SenderTitleDocflow
        Diadoc.Api.Proto.Docflow.ConfirmationDocflow
        Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow
        Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow
        Diadoc.Api.Proto.Docflow.AmendmentRequestDocflow
        Diadoc.Api.Proto.Docflow.RevocationDocflowV3
        Diadoc.Api.Proto.Docflow.RevocationRequestDocflow
        Diadoc.Api.Proto.Docflow.RevocationResponseDocflow
        Diadoc.Api.Proto.Docflow.ReceiptDocflowV3
        Diadoc.Api.Proto.Docflow.OuterDocflow
        Diadoc.Api.Proto.Docflow.OuterDocflowEntities
        Diadoc.Api.Proto.Docflow.StatusEntity
        Diadoc.Api.Proto.Docflow.ResolutionDocflowV3
        Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3
        Diadoc.Api.Proto.Docflow.ResolutionRequestV3
        Diadoc.Api.Proto.Docflow.ResolutionV3
        Diadoc.Api.Proto.Docflow.ApprovementSignatureV3
        Diadoc.Api.Proto.Docflow.SignatureDenialV3
        Diadoc.Api.Proto.Documents.DetectTitleResponse
        Diadoc.Api.Proto.Documents.DetectedDocumentTitle
        Diadoc.Api.Proto.Documents.DocumentList
        Diadoc.Api.Proto.Documents.DocumentProtocol
        Diadoc.Api.Proto.Documents.DocumentsMoveOperation
        Diadoc.Api.Proto.Documents.DocumentZipGenerationResult
        Diadoc.Api.Proto.Documents.Types.DetectedDocumentType
        Diadoc.Api.Proto.Documents.Types.DetectDocumentTypesResponse
        Diadoc.Api.Proto.Documents.Types.DocumentTypeDescriptionV2
        Diadoc.Api.Proto.Documents.Types.GetDocumentTypesResponseV2
        Diadoc.Api.Proto.Documents.Types.DocumentFunctionV2
        Diadoc.Api.Proto.Documents.Types.DocumentVersionV2
        Diadoc.Api.Proto.Documents.Types.DocumentWorkflowV2
        Diadoc.Api.Proto.Documents.Types.DocumentTitleV2
        Diadoc.Api.Proto.Documents.Types.SignerInfoV2
        Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2
        Diadoc.Api.Proto.Dss.DssSignRequest
        Diadoc.Api.Proto.Dss.DssSignFile
        Diadoc.Api.Proto.Dss.DssSignResult
        Diadoc.Api.Proto.Dss.DssFileSigningResult
        Diadoc.Api.Proto.Employees.Employee
        Diadoc.Api.Proto.Employees.EmployeePermissions
        Diadoc.Api.Proto.Employees.EmployeeAction
        Diadoc.Api.Proto.Employees.EmployeeList
        Diadoc.Api.Proto.User
        Diadoc.Api.Proto.UserV2
        Diadoc.Api.Proto.FullName
        Diadoc.Api.Proto.OrganizationUserPermissions
        Diadoc.Api.Proto.AuthorizationPermission
        Diadoc.Api.Proto.Employees.EmployeeToCreate
        Diadoc.Api.Proto.Employees.EmployeeToCreateCredentials
        Diadoc.Api.Proto.Employees.EmployeeToCreateByLogin
        Diadoc.Api.Proto.Employees.EmployeeToCreateByCertificate
        Diadoc.Api.Proto.Employees.EmployeeToUpdate
        Diadoc.Api.Proto.Employees.EmployeePermissionsPatch
        Diadoc.Api.Proto.Employees.EmployeeDepartmentPatch
        Diadoc.Api.Proto.Employees.EmployeeIsAdministratorPatch
        Diadoc.Api.Proto.Employees.EmployeeDocumentAccessLevelPatch
        Diadoc.Api.Proto.Employees.EmployeeSelectedDepartmentsPatch
        Diadoc.Api.Proto.Employees.EmployeePositionPatch
        Diadoc.Api.Proto.Employees.EmployeeCanBeInvitedForChatPatch
        Diadoc.Api.Proto.Employees.AuthorizationPermissionPatch
        Diadoc.Api.Proto.Employees.Subscriptions.EmployeeSubscriptions
        Diadoc.Api.Proto.Employees.Subscriptions.SubscriptionsToUpdate
        Diadoc.Api.Proto.Employees.Subscriptions.Subscription
        Diadoc.Api.Proto.Events.CancellationInfo
        Diadoc.Api.Proto.Events.BoxEventList
        Diadoc.Api.Proto.Events.BoxEvent
        Diadoc.Api.Proto.Events.Message
        Diadoc.Api.Proto.Events.Template
        Diadoc.Api.Proto.Events.MessagePatch
        Diadoc.Api.Proto.Events.Entity
        Diadoc.Api.Proto.Events.EntityPatch
        Diadoc.Api.Proto.Events.TemplateToLetterTransformationInfo
        Diadoc.Api.Proto.Events.TemplateTransformationInfo
        Diadoc.Api.Proto.Events.TemplateRefusalInfo
        Diadoc.Api.Proto.Events.ResolutionRequestDenialInfo
        Diadoc.Api.Proto.Events.ResolutionRouteAssignmentInfo
        Diadoc.Api.Proto.Events.ResolutionRouteRemovalInfo
        Diadoc.Api.Proto.Events.RevocationRequestInfo
        Diadoc.Api.Proto.ExternalServiceAuthInfo
        Diadoc.Api.Proto.Forwarding.ForwardedDocumentId
        Diadoc.Api.Proto.Forwarding.ForwardedDocument
        Diadoc.Api.Proto.Forwarding.ForwardDocumentRequest
        Diadoc.Api.Proto.Forwarding.ForwardDocumentResponse
        Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsRequest
        Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsResponse
        Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsRequest
        Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsResponse
        Diadoc.Api.Proto.Forwarding.ForwardedDocumentEvent
        Diadoc.Api.Proto.GetOrganizationsByInnListRequest
        Diadoc.Api.Proto.OrganizationWithCounteragentStatus
        Diadoc.Api.Proto.GetOrganizationsByInnListResponse
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552SellerTitleInfo
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552TransferInfo
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkDescription
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkItem
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552BuyerTitleInfo
        Diadoc.Api.Proto.Invoicing.TovTorgSellerTitleInfo
        Diadoc.Api.Proto.Invoicing.TovTorgBuyerTitleInfo
        Diadoc.Api.Proto.Invoicing.TovTorgTable
        Diadoc.Api.Proto.Invoicing.TovTorgItem
        Diadoc.Api.Proto.Invoicing.TovTorgTransferInfo
        Diadoc.Api.Proto.Invoicing.GroundInfo
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSellerTitleInfo
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificateBuyerTitleInfo
        Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo
        Diadoc.Api.Proto.Invoicing.WorkDescription
        Diadoc.Api.Proto.Invoicing.WorkItem
        Diadoc.Api.Proto.Invoicing.Official
        Diadoc.Api.Proto.Invoicing.Attorney
        Diadoc.Api.Proto.Invoicing.FnsRegistrationMessageInfo
        Diadoc.Api.Proto.Invoicing.InvoiceCorrectionRequestInfo
        Diadoc.Api.Proto.Invoicing.RevocationRequestInfo
        Diadoc.Api.Proto.Invoicing.SignatureRejectionInfo
        Diadoc.Api.Proto.Invoicing.Torg12SellerTitleInfo
        Diadoc.Api.Proto.Invoicing.Torg12BuyerTitleInfo
        Diadoc.Api.Proto.Invoicing.Torg12Item
        Diadoc.Api.Proto.Invoicing.Grounds
        Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry
        Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetRequest
        Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetResponse
        Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiPutRequest
        Diadoc.Api.Proto.LoginPassword
        Diadoc.Api.Proto.StringValue
        Diadoc.Api.Proto.OrganizationPropertiesToUpdate
        Diadoc.Api.Proto.HeadOrganizationPropertiesToUpdate
        Diadoc.Api.Proto.Organizations.AutoBlockStatus
        Diadoc.Api.Proto.Organizations.BlockStatus
        Diadoc.Api.Proto.Organizations.ManualBlockStatus
        Diadoc.Api.Proto.Organizations.OrganizationFeatures
        Diadoc.Api.Proto.OrganizationUser
        Diadoc.Api.Proto.OrganizationUsersList
        Diadoc.Api.Proto.Recognition.Recognized
        Diadoc.Api.Proto.Recognition.RecognizedInvoice
        Diadoc.Api.Proto.Registration.RegistrationRequest
        Diadoc.Api.Proto.Registration.RegistrationResponse
        Diadoc.Api.Proto.Registration.RegistrationConfirmRequest
        Diadoc.Api.Proto.ResolutionRouteList
        Diadoc.Api.Proto.ResolutionRoute
        Diadoc.Api.Proto.RoamingNotification
        Diadoc.Api.Proto.SignatureInfo
        Diadoc.Api.Proto.Users.UserToUpdate
        Diadoc.Api.Proto.Users.UserLoginPatch
        Diadoc.Api.Proto.Users.UserFullNamePatch

 =============================================================================}

interface

uses System.Classes, System.Generics.Collections, semin64.protobuf;

// Creating a TsanPBMessage object by full name
// Don't forget to call Free after using it
function CreateProtoInstance(ProtoName: string): TsanPBMessage;

implementation

const

  PROTO_MESSAGE_NAMES: array[0..387] of string = (
    'Diadoc.Api.Proto.AcquireCounteragentRequest',
    'Diadoc.Api.Proto.InvitationDocument',
    'Diadoc.Api.Proto.AcquireCounteragentResult',
    'Diadoc.Api.Proto.Events.MessageToPost',
    'Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment',
    'Diadoc.Api.Proto.Events.EncryptedInvoiceAttachment',
    'Diadoc.Api.Proto.Events.EncryptedDocumentMetadata',
    'Diadoc.Api.Proto.Events.EncryptedXmlBasicDocumentMetadata',
    'Diadoc.Api.Proto.Events.EncryptedInvoiceMetadata',
    'Diadoc.Api.Proto.Events.EncryptedInvoiceCorrectionMetadata',
    'Diadoc.Api.Proto.Events.XmlDocumentAttachment',
    'Diadoc.Api.Proto.Events.NonformalizedAttachment',
    'Diadoc.Api.Proto.Events.BasicDocumentAttachment',
    'Diadoc.Api.Proto.Events.Torg13Attachment',
    'Diadoc.Api.Proto.Events.AcceptanceCertificateAttachment',
    'Diadoc.Api.Proto.Events.TrustConnectionRequestAttachment',
    'Diadoc.Api.Proto.Events.StructuredDataAttachment',
    'Diadoc.Api.Proto.Events.PriceListAttachment',
    'Diadoc.Api.Proto.Events.ReconciliationActAttachment',
    'Diadoc.Api.Proto.Events.ContractAttachment',
    'Diadoc.Api.Proto.Events.SupplementaryAgreementAttachment',
    'Diadoc.Api.Proto.Events.ServiceDetailsAttachment',
    'Diadoc.Api.Proto.Events.DocumentAttachment',
    'Diadoc.Api.Proto.Events.MetadataItem',
    'Diadoc.Api.Proto.Events.MessagePatchToPost',
    'Diadoc.Api.Proto.Events.EditingPatch',
    'Diadoc.Api.Proto.Events.SignatureVerification',
    'Diadoc.Api.Proto.Events.ResolutionRequestAttachment',
    'Diadoc.Api.Proto.Events.ResolutionRouteAssignment',
    'Diadoc.Api.Proto.Events.ResolutionRequestCancellationAttachment',
    'Diadoc.Api.Proto.Events.ResolutionRequestDenialCancellationAttachment',
    'Diadoc.Api.Proto.Events.ResolutionRequestDenialAttachment',
    'Diadoc.Api.Proto.Events.ResolutionAttachment',
    'Diadoc.Api.Proto.Events.ReceiptAttachment',
    'Diadoc.Api.Proto.Events.RecipientTitleAttachment',
    'Diadoc.Api.Proto.Events.CorrectionRequestAttachment',
    'Diadoc.Api.Proto.Events.DocumentSignature',
    'Diadoc.Api.Proto.Events.DocumentSenderSignature',
    'Diadoc.Api.Proto.Events.RequestedSignatureRejection',
    'Diadoc.Api.Proto.Events.SignedContent',
    'Diadoc.Api.Proto.Events.DraftToSend',
    'Diadoc.Api.Proto.Events.PrepareDocumentsToSignRequest',
    'Diadoc.Api.Proto.Events.DraftDocumentToPatch',
    'Diadoc.Api.Proto.Events.ContentToPatch',
    'Diadoc.Api.Proto.Events.DocumentToPatch',
    'Diadoc.Api.Proto.Events.DocumentPatchedContent',
    'Diadoc.Api.Proto.Events.PrepareDocumentsToSignResponse',
    'Diadoc.Api.Proto.Events.MessageToSend',
    'Diadoc.Api.Proto.Events.RevocationRequestAttachment',
    'Diadoc.Api.Proto.Events.XmlSignatureRejectionAttachment',
    'Diadoc.Api.Proto.Events.RoamingNotificationToPost',
    'Diadoc.Api.Proto.Events.CustomDataPatch',
    'Diadoc.Api.Proto.Events.EditDocumentPacketCommand',
    'Diadoc.Api.Proto.Events.ResolutionRouteRemoval',
    'Diadoc.Api.Proto.Events.TemplateToPost',
    'Diadoc.Api.Proto.Events.TemplateDocumentAttachment',
    'Diadoc.Api.Proto.Events.TemplatePatchToPost',
    'Diadoc.Api.Proto.Events.TemplateRefusalAttachment',
    'Diadoc.Api.Proto.Events.PredefinedRecipientTitle',
    'Diadoc.Api.Proto.Events.UnsignedContent',
    'Diadoc.Api.Proto.Events.TemplateTransformationToPost',
    'Diadoc.Api.Proto.Events.DocumentTransformation',
    'Diadoc.Api.Proto.DocumentId',
    'Diadoc.Api.Proto.DocumentIdEx',
    'Diadoc.Api.Proto.CustomDataItem',
    'Diadoc.Api.Proto.Invoicing.Signer',
    'Diadoc.Api.Proto.Invoicing.SignerDetails',
    'Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner',
    'Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetails',
    'Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetailsToPost',
    'Diadoc.Api.Proto.Events.ResolutionInfo',
    'Diadoc.Api.Proto.Events.ResolutionRequestInfo',
    'Diadoc.Api.Proto.ResolutionTarget',
    'Diadoc.Api.Proto.Invoicing.DocflowParticipant',
    'Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo',
    'Diadoc.Api.Proto.Invoicing.OrganizationInfo',
    'Diadoc.Api.Proto.Address',
    'Diadoc.Api.Proto.RussianAddress',
    'Diadoc.Api.Proto.ForeignAddress',
    'Diadoc.Api.Proto.Docflow.DocumentInfo',
    'Diadoc.Api.Proto.Docflow.DocumentDateAndNumber',
    'Diadoc.Api.Proto.Docflow.BasicDocumentInfo',
    'Diadoc.Api.Proto.Docflow.InvoiceDocumentInfo',
    'Diadoc.Api.Proto.Docflow.InvoiceCorrectionDocumentInfo',
    'Diadoc.Api.Proto.Docflow.PriceListDocumentInfo',
    'Diadoc.Api.Proto.Docflow.ContractDocumentInfo',
    'Diadoc.Api.Proto.Docflow.SupplementaryAgreementDocumentInfo',
    'Diadoc.Api.Proto.Docflow.UniversalTransferDocumentInfo',
    'Diadoc.Api.Proto.Docflow.UniversalCorrectionDocumentInfo',
    'Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentSellerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.Shipper',
    'Diadoc.Api.Proto.Invoicing.InvoiceTable',
    'Diadoc.Api.Proto.Invoicing.ExtendedInvoiceItem',
    'Diadoc.Api.Proto.Invoicing.TransferInfo',
    'Diadoc.Api.Proto.Invoicing.TransferBase',
    'Diadoc.Api.Proto.Invoicing.Waybill',
    'Diadoc.Api.Proto.Invoicing.Employee',
    'Diadoc.Api.Proto.Invoicing.OtherIssuer',
    'Diadoc.Api.Proto.Invoicing.AdditionalInfoId',
    'Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentBuyerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.UniversalCorrectionDocumentSellerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.InvoiceForCorrectionInfo',
    'Diadoc.Api.Proto.Invoicing.InvoiceRevisionInfo',
    'Diadoc.Api.Proto.Invoicing.EventContent',
    'Diadoc.Api.Proto.Invoicing.CorrectionBase',
    'Diadoc.Api.Proto.Invoicing.InvoiceCorrectionTable',
    'Diadoc.Api.Proto.Invoicing.ExtendedInvoiceCorrectionItem',
    'Diadoc.Api.Proto.Invoicing.InvoiceInfo',
    'Diadoc.Api.Proto.Invoicing.AdditionalInfo',
    'Diadoc.Api.Proto.Invoicing.InvoiceItem',
    'Diadoc.Api.Proto.Invoicing.CustomsDeclaration',
    'Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo',
    'Diadoc.Api.Proto.Invoicing.ShipperOrConsignee',
    'Diadoc.Api.Proto.Invoicing.InvoiceCorrectionInfo',
    'Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff',
    'Diadoc.Api.Proto.Invoicing.InvoiceCorrectionItem',
    'Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields',
    'Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff',
    'Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo',
    'Diadoc.Api.Proto.AsyncMethodResult',
    'Diadoc.Api.Proto.CertificateInfo',
    'Diadoc.Api.Proto.Certificates.CertificateInfoV2',
    'Diadoc.Api.Proto.Certificates.CertificateList',
    'Diadoc.Api.Proto.CloudSignRequest',
    'Diadoc.Api.Proto.CloudSignFile',
    'Diadoc.Api.Proto.CloudSignResult',
    'Diadoc.Api.Proto.CloudSignConfirmResult',
    'Diadoc.Api.Proto.AutosignReceiptsResult',
    'Diadoc.Api.Proto.Content_v2',
    'Diadoc.Api.Proto.Content',
    'Diadoc.Api.Proto.Content_v3',
    'Diadoc.Api.Proto.CounteragentList',
    'Diadoc.Api.Proto.Counteragent',
    'Diadoc.Api.Proto.CounteragentCertificateList',
    'Diadoc.Api.Proto.Certificate',
    'Diadoc.Api.Proto.OrganizationList',
    'Diadoc.Api.Proto.Organization',
    'Diadoc.Api.Proto.Department',
    'Diadoc.Api.Proto.Box',
    'Diadoc.Api.Proto.CustomPrintFormDetectionRequest',
    'Diadoc.Api.Proto.CustomPrintFormDetectionResult',
    'Diadoc.Api.Proto.CustomPrintFormDetectionItemResult',
    'Diadoc.Api.Proto.Departments.Department',
    'Diadoc.Api.Proto.Timestamp',
    'Diadoc.Api.Proto.Departments.Routing',
    'Diadoc.Api.Proto.Departments.DepartmentList',
    'Diadoc.Api.Proto.Departments.DepartmentToCreate',
    'Diadoc.Api.Proto.Departments.DepartmentToUpdate',
    'Diadoc.Api.Proto.Departments.ParentDepartmentPatch',
    'Diadoc.Api.Proto.Departments.DepartmentNamingPatch',
    'Diadoc.Api.Proto.Departments.DepartmentKppPatch',
    'Diadoc.Api.Proto.Departments.DepartmentAddressPatch',
    'Diadoc.Api.Proto.Departments.DepartmentRoutingPatch',
    'Diadoc.Api.Proto.Docflow.Entity',
    'Diadoc.Api.Proto.Docflow.Attachment',
    'Diadoc.Api.Proto.Docflow.Signature',
    'Diadoc.Api.Proto.Docflow.SignedAttachment',
    'Diadoc.Api.Proto.SignatureVerificationResult',
    'Diadoc.Api.Proto.CertificateVerificationResult',
    'Diadoc.Api.Proto.CertificateChainElement',
    'Diadoc.Api.Proto.Docflow.SignatureV3',
    'Diadoc.Api.Proto.Docflow.SignedAttachmentV3',
    'Diadoc.Api.Proto.Docflow.BilateralDocflow',
    'Diadoc.Api.Proto.Docflow.ReceiptDocflow',
    'Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow',
    'Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow',
    'Diadoc.Api.Proto.Docflow.Docflow',
    'Diadoc.Api.Proto.Docflow.DocflowStatus',
    'Diadoc.Api.Proto.Docflow.DocflowStatusModel',
    'Diadoc.Api.Proto.Docflow.InboundInvoiceDocflow',
    'Diadoc.Api.Proto.Docflow.OutboundInvoiceDocflow',
    'Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow',
    'Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow',
    'Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow',
    'Diadoc.Api.Proto.Docflow.UnilateralDocflow',
    'Diadoc.Api.Proto.Docflow.XmlBilateralDocflow',
    'Diadoc.Api.Proto.Docflow.BuyerTitleDocflow',
    'Diadoc.Api.Proto.Docflow.RevocationDocflow',
    'Diadoc.Api.Proto.Docflow.ResolutionDocflow',
    'Diadoc.Api.Proto.Docflow.InboundUniversalTransferDocumentDocflow',
    'Diadoc.Api.Proto.Docflow.OutboundUniversalTransferDocumentDocflow',
    'Diadoc.Api.Proto.Docflow.RoamingNotification',
    'Diadoc.Api.Proto.Docflow.GetDocflowBatchRequest',
    'Diadoc.Api.Proto.Docflow.GetDocflowRequest',
    'Diadoc.Api.Proto.Docflow.GetDocflowBatchResponse',
    'Diadoc.Api.Proto.Docflow.SearchDocflowsRequest',
    'Diadoc.Api.Proto.Docflow.SearchDocflowsResponse',
    'Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdRequest',
    'Diadoc.Api.Proto.Docflow.FetchedDocument',
    'Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponse',
    'Diadoc.Api.Proto.Docflow.GetDocflowEventsRequest',
    'Diadoc.Api.Proto.Docflow.GetDocflowEventsResponse',
    'Diadoc.Api.Proto.Docflow.DocflowEvent',
    'Diadoc.Api.Proto.TimeBasedFilter',
    'Diadoc.Api.Proto.Docflow.DocumentWithDocflow',
    'Diadoc.Api.Proto.ForwardDocumentEvent',
    'Diadoc.Api.Proto.Docflow.GetDocflowBatchResponseV3',
    'Diadoc.Api.Proto.Docflow.SearchDocflowsResponseV3',
    'Diadoc.Api.Proto.Docflow.FetchedDocumentV3',
    'Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponseV3',
    'Diadoc.Api.Proto.Docflow.GetDocflowEventsResponseV3',
    'Diadoc.Api.Proto.Docflow.DocflowEventV3',
    'Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3',
    'Diadoc.Api.Proto.Docflow.LastEvent',
    'Diadoc.Api.Proto.Docflow.DocumentInfoV3',
    'Diadoc.Api.Proto.Docflow.DocumentParticipants',
    'Diadoc.Api.Proto.Docflow.DocumentParticipant',
    'Diadoc.Api.Proto.Docflow.DocumentLinks',
    'Diadoc.Api.Proto.Docflow.PacketInfo',
    'Diadoc.Api.Proto.Docflow.DocumentLetterInfo',
    'Diadoc.Api.Proto.Docflow.DocumentDraftInfo',
    'Diadoc.Api.Proto.Docflow.DocumentTemplateInfo',
    'Diadoc.Api.Proto.Docflow.TemplateTransformationInfo',
    'Diadoc.Api.Proto.Docflow.TemplateRefusalInfo',
    'Diadoc.Api.Proto.FullVersion',
    'Diadoc.Api.Proto.Documents.Document',
    'Diadoc.Api.Proto.Documents.LastOuterDocflow',
    'Diadoc.Api.Proto.Documents.ResolutionStatus',
    'Diadoc.Api.Proto.Documents.RecipientReceiptMetadata',
    'Diadoc.Api.Proto.Documents.SenderReceiptMetadata',
    'Diadoc.Api.Proto.Documents.ConfirmationMetadata',
    'Diadoc.Api.Proto.Documents.AmendmentRequestMetadata',
    'Diadoc.Api.Proto.Documents.Origin',
    'Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateMetadata',
    'Diadoc.Api.Proto.Documents.BilateralDocument.TrustConnectionRequestMetadata',
    'Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata',
    'Diadoc.Api.Proto.Documents.BilateralDocument.PriceListMetadata',
    'Diadoc.Api.Proto.Documents.BilateralDocument.ContractMetadata',
    'Diadoc.Api.Proto.Documents.BilateralDocument.SupplementaryAgreementMetadata',
    'Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentMetadata',
    'Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceMetadata',
    'Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceRevisionMetadata',
    'Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionMetadata',
    'Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionRevisionMetadata',
    'Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata',
    'Diadoc.Api.Proto.Documents.UnilateralDocument.ProformaInvoiceMetadata',
    'Diadoc.Api.Proto.Documents.UnilateralDocument.ServiceDetailsMetadata',
    'Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentMetadata',
    'Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentRevisionMetadata',
    'Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentMetadata',
    'Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentRevisionMetadata',
    'Diadoc.Api.Proto.OuterDocflowInfo',
    'Diadoc.Api.Proto.Status',
    'Diadoc.Api.Proto.StatusDetail',
    'Diadoc.Api.Proto.Docflow.DocflowV3',
    'Diadoc.Api.Proto.Docflow.SenderTitleDocflow',
    'Diadoc.Api.Proto.Docflow.ConfirmationDocflow',
    'Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow',
    'Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow',
    'Diadoc.Api.Proto.Docflow.AmendmentRequestDocflow',
    'Diadoc.Api.Proto.Docflow.RevocationDocflowV3',
    'Diadoc.Api.Proto.Docflow.RevocationRequestDocflow',
    'Diadoc.Api.Proto.Docflow.RevocationResponseDocflow',
    'Diadoc.Api.Proto.Docflow.ReceiptDocflowV3',
    'Diadoc.Api.Proto.Docflow.OuterDocflow',
    'Diadoc.Api.Proto.Docflow.OuterDocflowEntities',
    'Diadoc.Api.Proto.Docflow.StatusEntity',
    'Diadoc.Api.Proto.Docflow.ResolutionDocflowV3',
    'Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3',
    'Diadoc.Api.Proto.Docflow.ResolutionRequestV3',
    'Diadoc.Api.Proto.Docflow.ResolutionV3',
    'Diadoc.Api.Proto.Docflow.ApprovementSignatureV3',
    'Diadoc.Api.Proto.Docflow.SignatureDenialV3',
    'Diadoc.Api.Proto.Documents.DetectTitleResponse',
    'Diadoc.Api.Proto.Documents.DetectedDocumentTitle',
    'Diadoc.Api.Proto.Documents.DocumentList',
    'Diadoc.Api.Proto.Documents.DocumentProtocol',
    'Diadoc.Api.Proto.Documents.DocumentsMoveOperation',
    'Diadoc.Api.Proto.Documents.DocumentZipGenerationResult',
    'Diadoc.Api.Proto.Documents.Types.DetectedDocumentType',
    'Diadoc.Api.Proto.Documents.Types.DetectDocumentTypesResponse',
    'Diadoc.Api.Proto.Documents.Types.DocumentTypeDescriptionV2',
    'Diadoc.Api.Proto.Documents.Types.GetDocumentTypesResponseV2',
    'Diadoc.Api.Proto.Documents.Types.DocumentFunctionV2',
    'Diadoc.Api.Proto.Documents.Types.DocumentVersionV2',
    'Diadoc.Api.Proto.Documents.Types.DocumentWorkflowV2',
    'Diadoc.Api.Proto.Documents.Types.DocumentTitleV2',
    'Diadoc.Api.Proto.Documents.Types.SignerInfoV2',
    'Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2',
    'Diadoc.Api.Proto.Dss.DssSignRequest',
    'Diadoc.Api.Proto.Dss.DssSignFile',
    'Diadoc.Api.Proto.Dss.DssSignResult',
    'Diadoc.Api.Proto.Dss.DssFileSigningResult',
    'Diadoc.Api.Proto.Employees.Employee',
    'Diadoc.Api.Proto.Employees.EmployeePermissions',
    'Diadoc.Api.Proto.Employees.EmployeeAction',
    'Diadoc.Api.Proto.Employees.EmployeeList',
    'Diadoc.Api.Proto.User',
    'Diadoc.Api.Proto.UserV2',
    'Diadoc.Api.Proto.FullName',
    'Diadoc.Api.Proto.OrganizationUserPermissions',
    'Diadoc.Api.Proto.AuthorizationPermission',
    'Diadoc.Api.Proto.Employees.EmployeeToCreate',
    'Diadoc.Api.Proto.Employees.EmployeeToCreateCredentials',
    'Diadoc.Api.Proto.Employees.EmployeeToCreateByLogin',
    'Diadoc.Api.Proto.Employees.EmployeeToCreateByCertificate',
    'Diadoc.Api.Proto.Employees.EmployeeToUpdate',
    'Diadoc.Api.Proto.Employees.EmployeePermissionsPatch',
    'Diadoc.Api.Proto.Employees.EmployeeDepartmentPatch',
    'Diadoc.Api.Proto.Employees.EmployeeIsAdministratorPatch',
    'Diadoc.Api.Proto.Employees.EmployeeDocumentAccessLevelPatch',
    'Diadoc.Api.Proto.Employees.EmployeeSelectedDepartmentsPatch',
    'Diadoc.Api.Proto.Employees.EmployeePositionPatch',
    'Diadoc.Api.Proto.Employees.EmployeeCanBeInvitedForChatPatch',
    'Diadoc.Api.Proto.Employees.AuthorizationPermissionPatch',
    'Diadoc.Api.Proto.Employees.Subscriptions.EmployeeSubscriptions',
    'Diadoc.Api.Proto.Employees.Subscriptions.SubscriptionsToUpdate',
    'Diadoc.Api.Proto.Employees.Subscriptions.Subscription',
    'Diadoc.Api.Proto.Events.CancellationInfo',
    'Diadoc.Api.Proto.Events.BoxEventList',
    'Diadoc.Api.Proto.Events.BoxEvent',
    'Diadoc.Api.Proto.Events.Message',
    'Diadoc.Api.Proto.Events.Template',
    'Diadoc.Api.Proto.Events.MessagePatch',
    'Diadoc.Api.Proto.Events.Entity',
    'Diadoc.Api.Proto.Events.EntityPatch',
    'Diadoc.Api.Proto.Events.TemplateToLetterTransformationInfo',
    'Diadoc.Api.Proto.Events.TemplateTransformationInfo',
    'Diadoc.Api.Proto.Events.TemplateRefusalInfo',
    'Diadoc.Api.Proto.Events.ResolutionRequestDenialInfo',
    'Diadoc.Api.Proto.Events.ResolutionRouteAssignmentInfo',
    'Diadoc.Api.Proto.Events.ResolutionRouteRemovalInfo',
    'Diadoc.Api.Proto.Events.RevocationRequestInfo',
    'Diadoc.Api.Proto.ExternalServiceAuthInfo',
    'Diadoc.Api.Proto.Forwarding.ForwardedDocumentId',
    'Diadoc.Api.Proto.Forwarding.ForwardedDocument',
    'Diadoc.Api.Proto.Forwarding.ForwardDocumentRequest',
    'Diadoc.Api.Proto.Forwarding.ForwardDocumentResponse',
    'Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsRequest',
    'Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsResponse',
    'Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsRequest',
    'Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsResponse',
    'Diadoc.Api.Proto.Forwarding.ForwardedDocumentEvent',
    'Diadoc.Api.Proto.GetOrganizationsByInnListRequest',
    'Diadoc.Api.Proto.OrganizationWithCounteragentStatus',
    'Diadoc.Api.Proto.GetOrganizationsByInnListResponse',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552SellerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552TransferInfo',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkDescription',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkItem',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552BuyerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.TovTorgSellerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.TovTorgBuyerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.TovTorgTable',
    'Diadoc.Api.Proto.Invoicing.TovTorgItem',
    'Diadoc.Api.Proto.Invoicing.TovTorgTransferInfo',
    'Diadoc.Api.Proto.Invoicing.GroundInfo',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSellerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificateBuyerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo',
    'Diadoc.Api.Proto.Invoicing.WorkDescription',
    'Diadoc.Api.Proto.Invoicing.WorkItem',
    'Diadoc.Api.Proto.Invoicing.Official',
    'Diadoc.Api.Proto.Invoicing.Attorney',
    'Diadoc.Api.Proto.Invoicing.FnsRegistrationMessageInfo',
    'Diadoc.Api.Proto.Invoicing.InvoiceCorrectionRequestInfo',
    'Diadoc.Api.Proto.Invoicing.RevocationRequestInfo',
    'Diadoc.Api.Proto.Invoicing.SignatureRejectionInfo',
    'Diadoc.Api.Proto.Invoicing.Torg12SellerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.Torg12BuyerTitleInfo',
    'Diadoc.Api.Proto.Invoicing.Torg12Item',
    'Diadoc.Api.Proto.Invoicing.Grounds',
    'Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry',
    'Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetRequest',
    'Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetResponse',
    'Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiPutRequest',
    'Diadoc.Api.Proto.LoginPassword',
    'Diadoc.Api.Proto.StringValue',
    'Diadoc.Api.Proto.OrganizationPropertiesToUpdate',
    'Diadoc.Api.Proto.HeadOrganizationPropertiesToUpdate',
    'Diadoc.Api.Proto.Organizations.AutoBlockStatus',
    'Diadoc.Api.Proto.Organizations.BlockStatus',
    'Diadoc.Api.Proto.Organizations.ManualBlockStatus',
    'Diadoc.Api.Proto.Organizations.OrganizationFeatures',
    'Diadoc.Api.Proto.OrganizationUser',
    'Diadoc.Api.Proto.OrganizationUsersList',
    'Diadoc.Api.Proto.Recognition.Recognized',
    'Diadoc.Api.Proto.Recognition.RecognizedInvoice',
    'Diadoc.Api.Proto.Registration.RegistrationRequest',
    'Diadoc.Api.Proto.Registration.RegistrationResponse',
    'Diadoc.Api.Proto.Registration.RegistrationConfirmRequest',
    'Diadoc.Api.Proto.ResolutionRouteList',
    'Diadoc.Api.Proto.ResolutionRoute',
    'Diadoc.Api.Proto.RoamingNotification',
    'Diadoc.Api.Proto.SignatureInfo',
    'Diadoc.Api.Proto.Users.UserToUpdate',
    'Diadoc.Api.Proto.Users.UserLoginPatch',
    'Diadoc.Api.Proto.Users.UserFullNamePatch'
  );

  PROTO_ENUM_NAMES: array[0..50] of string = (
    'Diadoc.Api.Proto.Events.CustomDataPatchOperation',
    'Diadoc.Api.Proto.LockMode',
    'Diadoc.Api.Proto.ResolutionRequestType',
    'Diadoc.Api.Proto.ResolutionType',
    'Diadoc.Api.Proto.Invoicing.Signers.SignerType',
    'Diadoc.Api.Proto.Invoicing.Signers.SignerPowers',
    'Diadoc.Api.Proto.Invoicing.Signers.SignerStatus',
    'Diadoc.Api.Proto.Invoicing.Signers.DocumentTitleType',
    'Diadoc.Api.Proto.ResolutionAction',
    'Diadoc.Api.Proto.DocumentType',
    'Diadoc.Api.Proto.DocumentDirection',
    'Diadoc.Api.Proto.Invoicing.FunctionType',
    'Diadoc.Api.Proto.Invoicing.ItemMark',
    'Diadoc.Api.Proto.Invoicing.InvoiceFormatVersion',
    'Diadoc.Api.Proto.Invoicing.TaxRate',
    'Diadoc.Api.Proto.Invoicing.Organizations.OrgType',
    'Diadoc.Api.Proto.Certificates.CertificateType',
    'Diadoc.Api.Proto.CounteragentStatus',
    'Diadoc.Api.Proto.OrganizationInvoiceFormatVersion',
    'Diadoc.Api.Proto.Sociability',
    'Diadoc.Api.Proto.Docflow.DocflowStatusSeverity',
    'Diadoc.Api.Proto.Docflow.SearchScope',
    'Diadoc.Api.Proto.TotalCountType',
    'Diadoc.Api.Proto.SortDirection',
    'Diadoc.Api.Proto.Documents.ResolutionStatusType',
    'Diadoc.Api.Proto.Documents.RevocationStatus',
    'Diadoc.Api.Proto.Documents.RoamingNotificationStatus',
    'Diadoc.Api.Proto.Documents.SenderSignatureStatus',
    'Diadoc.Api.Proto.Documents.ProxySignatureStatus',
    'Diadoc.Api.Proto.Documents.GeneralReceiptStatus',
    'Diadoc.Api.Proto.Documents.RecipientResponseStatus',
    'Diadoc.Api.Proto.Documents.MessageType',
    'Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateDocumentStatus',
    'Diadoc.Api.Proto.Documents.ReceiptStatus',
    'Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus',
    'Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus',
    'Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentStatus',
    'Diadoc.Api.Proto.Documents.UnilateralDocument.UnilateralDocumentStatus',
    'Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus',
    'Diadoc.Api.Proto.OuterStatusType',
    'Diadoc.Api.Proto.Docflow.ResolutionStatus',
    'Diadoc.Api.Proto.Dss.DssConfirmType',
    'Diadoc.Api.Proto.Dss.DssOperator',
    'Diadoc.Api.Proto.Dss.DssFileSigningStatus',
    'Diadoc.Api.Proto.Dss.DssOperationStatus',
    'Diadoc.Api.Proto.DocumentAccessLevel',
    'Diadoc.Api.Proto.Events.TemplateRefusalType',
    'Diadoc.Api.Proto.Events.EntityType',
    'Diadoc.Api.Proto.Events.AttachmentType',
    'Diadoc.Api.Proto.Recognition.RecognizedDocumentType',
    'Diadoc.Api.Proto.Registration.RegistrationStatus'
  );

var
  ProtoTypeList: TList<TsanPBCustomType>;

function GetProtoType(ProtoTypeName: string): TsanPBCustomType;
var
  ProtoType: TsanPBCustomType;
begin
  Result:= nil;
  for ProtoType in ProtoTypeList do begin
    if ProtoType.Name = ProtoTypeName then begin
      Result:= ProtoType;
      break;
    end;
  end;
end;

function CreateProtoInstance(ProtoName: string): TsanPBMessage;
var
  ProtoType: TsanPBCustomType;
begin
  ProtoType:= GetProtoType(ProtoName);
  if Assigned(ProtoType) and (ProtoType is TsanPBMessageType) then begin
    Result:= TsanPBMessageType(ProtoType).CreateInstance;
  end else begin
    Result:= nil;
  end;
end;

function GetMapType(KeyType: TsanPBFieldType; ValueType: TsanPBFieldType;
  ValueCustomType: TsanPBCustomType): TsanPBMessageType;
begin
  Result:= TsanPBMessageType.Create(nil, '');
  Result.AddFieldDef(ftoRequired, KeyType, nil, 'key', 1);
  Result.AddFieldDef(ftoRequired, ValueType, ValueCustomType, 'value', 2);
  ProtoTypeList.Add(Result);
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\AcquireCounteragent.proto
// Diadoc.Api.Proto.AcquireCounteragentRequest
procedure DefineMessageFields_1;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.AcquireCounteragentRequest')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'OrgId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageToCounteragent', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.InvitationDocument'), 'InvitationDocument', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\AcquireCounteragent.proto
// Diadoc.Api.Proto.InvitationDocument
procedure DefineMessageFields_2;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.InvitationDocument')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'SignatureRequested', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'Type', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\AcquireCounteragent.proto
// Diadoc.Api.Proto.AcquireCounteragentResult
procedure DefineMessageFields_3;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.AcquireCounteragentResult')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'OrgId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InvitationDocumentId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.MessageToPost
procedure DefineMessageFields_4;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.MessageToPost')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'FromBoxId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ToBoxId', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.XmlDocumentAttachment'), 'Invoices', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.NonformalizedAttachment'), 'NonformalizedDocuments', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.BasicDocumentAttachment'), 'Torg12Documents', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.AcceptanceCertificateAttachment'), 'AcceptanceCertificates', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.StructuredDataAttachment'), 'StructuredDataAttachments', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.BasicDocumentAttachment'), 'ProformaInvoices', 9);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.XmlDocumentAttachment'), 'XmlTorg12SellerTitles', 10);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.XmlDocumentAttachment'), 'XmlAcceptanceCertificateSellerTitles', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'ToDepartmentId', 12);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDraft', 13);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'LockDraft', 14);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'StrictDraftValidation', 15);
    FieldDef[FieldDefsCount-1].DefaultValue:= True;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsInternal', 16);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'FromDepartmentId', 17);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DelaySend', 18);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.PriceListAttachment'), 'PriceLists', 19);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.NonformalizedAttachment'), 'PriceListAgreements', 20);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.NonformalizedAttachment'), 'CertificateRegistries', 21);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ReconciliationActAttachment'), 'ReconciliationActs', 22);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ContractAttachment'), 'Contracts', 23);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.Torg13Attachment'), 'Torg13Documents', 24);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ServiceDetailsAttachment'), 'ServiceDetailsDocuments', 25);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyBoxId', 26);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyDepartmentId', 27);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedInvoiceAttachment'), 'EncryptedInvoices', 28);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment'), 'EncryptedXmlTorg12SellerTitles', 29);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment'), 'EncryptedXmlAcceptanceCertificateSellerTitles', 30);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SupplementaryAgreementAttachment'), 'SupplementaryAgreements', 31);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'LockPacket', 32);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.XmlDocumentAttachment'), 'UniversalTransferDocumentSellerTitles', 33);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentAttachment'), 'DocumentAttachments', 34);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.LockMode'), 'LockMode', 35);
    FieldDef[FieldDefsCount-1].DefaultValue:= 1;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment
procedure DefineMessageFields_5;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 7);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedDocumentMetadata'), 'Metadata', 8);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedXmlBasicDocumentMetadata'), 'XmlBasicMetadata', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EncryptedInvoiceAttachment
procedure DefineMessageFields_6;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EncryptedInvoiceAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 7);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedDocumentMetadata'), 'Metadata', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedInvoiceMetadata'), 'InvoiceMetadata', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EncryptedInvoiceCorrectionMetadata'), 'InvoiceCorrectionMetadata', 10);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EncryptedDocumentMetadata
procedure DefineMessageFields_7;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EncryptedDocumentMetadata')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'FileId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'BuyerFnsParticipantId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'SenderFnsParticipantId', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'DocumentDateAndNumber', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EncryptedXmlBasicDocumentMetadata
procedure DefineMessageFields_8;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EncryptedXmlBasicDocumentMetadata')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'FormationDate', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FormationTime', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentName', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EncryptedInvoiceMetadata
procedure DefineMessageFields_9;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EncryptedInvoiceMetadata')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'RevisionDateAndNumber', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EncryptedInvoiceCorrectionMetadata
procedure DefineMessageFields_10;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EncryptedInvoiceCorrectionMetadata')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalInvoiceDateAndNumber', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalInvoiceRevisionDateAndNumber', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'InvoiceCorrectionRevisionDateAndNumber', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.XmlDocumentAttachment
procedure DefineMessageFields_11;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.XmlDocumentAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.NonformalizedAttachment
procedure DefineMessageFields_12;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.NonformalizedAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedRecipientSignature', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentDate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 11);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.BasicDocumentAttachment
procedure DefineMessageFields_13;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.BasicDocumentAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.Torg13Attachment
procedure DefineMessageFields_14;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.Torg13Attachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.AcceptanceCertificateAttachment
procedure DefineMessageFields_15;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.AcceptanceCertificateAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= '';
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedRecipientSignature', 13);
    FieldDef[FieldDefsCount-1].DefaultValue:= True;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 14);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.TrustConnectionRequestAttachment
procedure DefineMessageFields_16;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TrustConnectionRequestAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.StructuredDataAttachment
procedure DefineMessageFields_17;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.StructuredDataAttachment')) do begin
    AddFieldDef(ftoRequired, ftBytes, nil, 'Content', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'ParentCustomDocumentId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.PriceListAttachment
procedure DefineMessageFields_18;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.PriceListAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 8);
    AddFieldDef(ftoRequired, ftString, nil, 'PriceListEffectiveDate', 9);
    AddFieldDef(ftoRequired, ftString, nil, 'ContractDocumentDate', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'ContractDocumentNumber', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ReconciliationActAttachment
procedure DefineMessageFields_19;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ReconciliationActAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 8);
    FieldDef[FieldDefsCount-1].DefaultValue:= '';
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 11);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ContractAttachment
procedure DefineMessageFields_20;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ContractAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractPrice', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractType', 10);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 11);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 12);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.SupplementaryAgreementAttachment
procedure DefineMessageFields_21;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.SupplementaryAgreementAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 9);
    AddFieldDef(ftoRequired, ftString, nil, 'ContractNumber', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'ContractDate', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractType', 12);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 13);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 14);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ServiceDetailsAttachment
procedure DefineMessageFields_22;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ServiceDetailsAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FileName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentDate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 11);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DocumentAttachment
procedure DefineMessageFields_23;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DocumentAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedRecipientSignature', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'Function', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'Version', 14);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.MetadataItem'), 'Metadata', 15);
    AddFieldDef(ftoOptional, ftInt32, nil, 'WorkflowId', 16);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsEncrypted', 17);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'EditingSettingId', 18);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.MetadataItem
procedure DefineMessageFields_24;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.MetadataItem')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Key', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Value', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.MessagePatchToPost
procedure DefineMessageFields_25;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.MessagePatchToPost')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ReceiptAttachment'), 'Receipts', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.CorrectionRequestAttachment'), 'CorrectionRequests', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentSignature'), 'Signatures', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RequestedSignatureRejection'), 'RequestedSignatureRejections', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RecipientTitleAttachment'), 'XmlTorg12BuyerTitles', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RecipientTitleAttachment'), 'XmlAcceptanceCertificateBuyerTitles', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionAttachment'), 'Resolutions', 9);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestAttachment'), 'ResolutionRequests', 10);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestCancellationAttachment'), 'ResolutionRequestCancellations', 11);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestDenialAttachment'), 'ResolutionRequestDenials', 12);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestDenialCancellationAttachment'), 'ResolutionRequestDenialCancellations', 13);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RevocationRequestAttachment'), 'RevocationRequests', 14);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.XmlSignatureRejectionAttachment'), 'XmlSignatureRejections', 15);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.CustomDataPatch'), 'CustomDataPatches', 16);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteAssignment'), 'ResolutionRouteAssignments', 17);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignatureVerification'), 'SignatureVerifications', 18);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EditDocumentPacketCommand'), 'EditDocumentPacketCommands', 19);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RecipientTitleAttachment'), 'UniversalTransferDocumentBuyerTitles', 20);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteRemoval'), 'ResolutionRouteRemovals', 21);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RecipientTitleAttachment'), 'RecipientTitles', 22);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EditingPatch'), 'EditingPatches', 24);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EditingPatch
procedure DefineMessageFields_26;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EditingPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.UnsignedContent'), 'Content', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.SignatureVerification
procedure DefineMessageFields_27;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.SignatureVerification')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialDocumentId', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsValid', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ErrorMessage', 3);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionRequestAttachment
procedure DefineMessageFields_28;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialDocumentId', 1);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionRequestType'), 'Type', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'TargetUserId', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'TargetDepartmentId', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 5);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionRouteAssignment
procedure DefineMessageFields_29;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteAssignment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialDocumentId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'RouteId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionRequestCancellationAttachment
procedure DefineMessageFields_30;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestCancellationAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialResolutionRequestId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionRequestDenialCancellationAttachment
procedure DefineMessageFields_31;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestDenialCancellationAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialResolutionRequestDenialId', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionRequestDenialAttachment
procedure DefineMessageFields_32;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestDenialAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialResolutionRequestId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionAttachment
procedure DefineMessageFields_33;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitialDocumentId', 1);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionType'), 'ResolutionType', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ReceiptAttachment
procedure DefineMessageFields_34;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ReceiptAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.RecipientTitleAttachment
procedure DefineMessageFields_35;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.RecipientTitleAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'NeedReceipt', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.CorrectionRequestAttachment
procedure DefineMessageFields_36;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.CorrectionRequestAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DocumentSignature
procedure DefineMessageFields_37;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DocumentSignature')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'Signature', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'SignWithTestSignature', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsApprovementSignature', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'SignatureNameOnShelf', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'PatchedContentId', 7);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DocumentSenderSignature
procedure DefineMessageFields_38;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DocumentSenderSignature')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'Signature', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'SignWithTestSignature', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'PatchedContentId', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.RequestedSignatureRejection
procedure DefineMessageFields_39;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.RequestedSignatureRejection')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.SignedContent
procedure DefineMessageFields_40;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.SignedContent')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'Content', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'Signature', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'NameOnShelf', 4);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'SignWithTestSignature', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'SignatureNameOnShelf', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DraftToSend
procedure DefineMessageFields_41;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DraftToSend')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'DraftId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ToBoxId', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'ToDepartmentId', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentSenderSignature'), 'DocumentSignatures', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyBoxId', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyDepartmentId', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.PrepareDocumentsToSignRequest
procedure DefineMessageFields_42;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.PrepareDocumentsToSignRequest')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DraftDocumentToPatch'), 'DraftDocuments', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentToPatch'), 'Documents', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ContentToPatch'), 'Contents', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DraftDocumentToPatch
procedure DefineMessageFields_43;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DraftDocumentToPatch')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ToBoxId', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'ExtendedSigner', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ContentToPatch
procedure DefineMessageFields_44;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ContentToPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Function', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.UnsignedContent'), 'Content', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'ToBoxId', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'ExtendedSigner', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DocumentToPatch
procedure DefineMessageFields_45;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DocumentToPatch')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'ExtendedSigner', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DocumentPatchedContent
procedure DefineMessageFields_46;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DocumentPatchedContent')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'PatchedContentId', 2);
    AddFieldDef(ftoOptional, ftBytes, nil, 'Content', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.PrepareDocumentsToSignResponse
procedure DefineMessageFields_47;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.PrepareDocumentsToSignResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentPatchedContent'), 'DocumentPatchedContents', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.MessageToSend
procedure DefineMessageFields_48;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.MessageToSend')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentSignature'), 'DocumentSignatures', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.RevocationRequestAttachment
procedure DefineMessageFields_49;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.RevocationRequestAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.XmlSignatureRejectionAttachment
procedure DefineMessageFields_50;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.XmlSignatureRejectionAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.SignedContent'), 'SignedContent', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.RoamingNotificationToPost
procedure DefineMessageFields_51;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.RoamingNotificationToPost')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'EventId', 2);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'Success', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Description', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageId', 5);
    AddFieldDef(ftoRepeated, ftString, nil, 'NotifiableEntityIds', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.CustomDataPatch
procedure DefineMessageFields_52;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.CustomDataPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Events.CustomDataPatchOperation'), 'Operation', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Key', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Value', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.EditDocumentPacketCommand
procedure DefineMessageFields_53;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EditDocumentPacketCommand')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentId', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'AddDocumentsToPacket', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'RemoveDocumentsFromPacket', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.ResolutionRouteRemoval
procedure DefineMessageFields_54;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteRemoval')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'RouteId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= '';
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.TemplateToPost
procedure DefineMessageFields_55;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateToPost')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'FromBoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'ToBoxId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageFromBoxId', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageToBoxId', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageToDepartmentId', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.TemplateDocumentAttachment'), 'DocumentAttachments', 6);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.LockMode'), 'LockMode', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= 1;
    AddFieldDef(ftoOptional, ftString, nil, 'FromDepartmentId', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'ToDepartmentId', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageProxyBoxId', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageProxyDepartmentId', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReusable', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.TemplateDocumentAttachment
procedure DefineMessageFields_56;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateDocumentAttachment')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.UnsignedContent'), 'UnsignedContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Function', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Version', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.MetadataItem'), 'Metadata', 6);
    AddFieldDef(ftoOptional, ftInt32, nil, 'WorkflowId', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'EditingSettingId', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedRecipientSignature', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.PredefinedRecipientTitle'), 'PredefinedRecipientTitle', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'RefusalDisabled', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.TemplatePatchToPost
procedure DefineMessageFields_57;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplatePatchToPost')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.TemplateRefusalAttachment'), 'Refusals', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.TemplateRefusalAttachment
procedure DefineMessageFields_58;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateRefusalAttachment')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.PredefinedRecipientTitle
procedure DefineMessageFields_59;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.PredefinedRecipientTitle')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.UnsignedContent'), 'UnsignedContent', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.UnsignedContent
procedure DefineMessageFields_60;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.UnsignedContent')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'Content', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'NameOnShelf', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.TemplateTransformationToPost
procedure DefineMessageFields_61;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateTransformationToPost')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'TemplateId', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.DocumentTransformation'), 'DocumentTransformations', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.DocumentTransformation
procedure DefineMessageFields_62;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.DocumentTransformation')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentId.proto
// Diadoc.Api.Proto.DocumentId
procedure DefineMessageFields_63;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.DocumentId')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'EntityId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentId.proto
// Diadoc.Api.Proto.DocumentIdEx
procedure DefineMessageFields_64;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.DocumentIdEx')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'EntityId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CustomDataItem.proto
// Diadoc.Api.Proto.CustomDataItem
procedure DefineMessageFields_65;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CustomDataItem')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Key', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Value', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Signer.proto
// Diadoc.Api.Proto.Invoicing.Signer
procedure DefineMessageFields_66;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signer')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'SignerCertificate', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.SignerDetails'), 'SignerDetails', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerCertificateThumbprint', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Signer.proto
// Diadoc.Api.Proto.Invoicing.SignerDetails
procedure DefineMessageFields_67;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.SignerDetails')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Surname', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FirstName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Patronymic', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'JobTitle', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'SoleProprietorRegistrationCertificate', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner
procedure DefineMessageFields_68;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'SignerCertificate', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerCertificateThumbprint', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetails'), 'SignerDetails', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetails
procedure DefineMessageFields_69;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetails')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Surname', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FirstName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Patronymic', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'JobTitle', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'RegistrationCertificate', 6);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerType'), 'SignerType', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= 1;
    AddFieldDef(ftoOptional, ftString, nil, 'SignerOrganizationName', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerInfo', 9);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerPowers'), 'SignerPowers', 10);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerStatus'), 'SignerStatus', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerPowersBase', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerOrgPowersBase', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetailsToPost
procedure DefineMessageFields_70;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetailsToPost')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'JobTitle', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'RegistrationCertificate', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerType'), 'SignerType', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerInfo', 4);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerPowers'), 'SignerPowers', 5);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerStatus'), 'SignerStatus', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerPowersBase', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerOrgPowersBase', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionInfo.proto
// Diadoc.Api.Proto.Events.ResolutionInfo
procedure DefineMessageFields_71;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionInfo')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionType'), 'ResolutionType', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoRequired, ftString, nil, 'Author', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'InitialRequestId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRequestInfo.proto
// Diadoc.Api.Proto.Events.ResolutionRequestInfo
procedure DefineMessageFields_72;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestInfo')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionRequestType'), 'RequestType', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoRequired, ftString, nil, 'Author', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.ResolutionTarget'), 'Target', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'ResolvedWith', 4);
    AddFieldDef(ftoRepeated, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionAction'), 'Actions', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionTarget.proto
// Diadoc.Api.Proto.ResolutionTarget
procedure DefineMessageFields_73;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.ResolutionTarget')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Department', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DepartmentId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'User', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'UserId', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\OrganizationInfo.proto
// Diadoc.Api.Proto.Invoicing.DocflowParticipant
procedure DefineMessageFields_74;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.DocflowParticipant')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'FnsParticipantId', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\OrganizationInfo.proto
// Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo
procedure DefineMessageFields_75;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo'), 'OrgInfo', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\OrganizationInfo.proto
// Diadoc.Api.Proto.Invoicing.OrganizationInfo
procedure DefineMessageFields_76;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 4);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsSoleProprietor', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'Okopf', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'Okpo', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Okdp', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Phone', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Fax', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'BankAccountNumber', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'BankName', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'BankId', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'Department', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'FnsParticipantId', 15);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Address.proto
// Diadoc.Api.Proto.Address
procedure DefineMessageFields_77;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Address')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.RussianAddress'), 'RussianAddress', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.ForeignAddress'), 'ForeignAddress', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'AddressCode', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Address.proto
// Diadoc.Api.Proto.RussianAddress
procedure DefineMessageFields_78;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.RussianAddress')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ZipCode', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Region', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Territory', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'City', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Locality', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Street', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'Building', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Block', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Apartment', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Address.proto
// Diadoc.Api.Proto.ForeignAddress
procedure DefineMessageFields_79;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.ForeignAddress')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Country', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Address', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.DocumentInfo
procedure DefineMessageFields_80;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentType'), 'DocumentType', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentDirection'), 'DocumentDirection', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsTest', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'FromDepartmentId', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'ToDepartmentId', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'CounteragentBoxId', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'DocumentDateAndNumber', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.BasicDocumentInfo'), 'BasicDocumentInfo', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceDocumentInfo'), 'InvoiceInfo', 10);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionDocumentInfo'), 'InvoiceCorrectionInfo', 11);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.PriceListDocumentInfo'), 'PriceListInfo', 12);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ContractDocumentInfo'), 'ContractInfo', 13);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SupplementaryAgreementDocumentInfo'), 'SupplementaryAgreementInfo', 14);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.UniversalTransferDocumentInfo'), 'UniversalTransferDocumentInfo', 15);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.UniversalCorrectionDocumentInfo'), 'UniversalCorrectionDocumentInfo', 16);
    AddFieldDef(ftoOptional, ftString, nil, 'AttachmentVersion', 17);
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 18);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.DocumentDateAndNumber
procedure DefineMessageFields_81;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentDate', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.BasicDocumentInfo
procedure DefineMessageFields_82;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.BasicDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 1);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NoVat', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'RevisionDateAndNumber', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.InvoiceDocumentInfo
procedure DefineMessageFields_83;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 2);
    AddFieldDef(ftoOptional, ftInt32, nil, 'CurrencyCode', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalInvoiceDateAndNumber', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.InvoiceCorrectionDocumentInfo
procedure DefineMessageFields_84;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'TotalInc', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalDec', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'VatInc', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'VatDec', 4);
    AddFieldDef(ftoOptional, ftInt32, nil, 'CurrencyCode', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalInvoiceDateAndNumber', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalInvoiceRevisionDateAndNumber', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalInvoiceCorrectionDateAndNumber', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.PriceListDocumentInfo
procedure DefineMessageFields_85;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.PriceListDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'PriceListEffectiveDate', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'ContractDocumentDateAndNumber', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.ContractDocumentInfo
procedure DefineMessageFields_86;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ContractDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ContractPrice', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractType', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.SupplementaryAgreementDocumentInfo
procedure DefineMessageFields_87;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SupplementaryAgreementDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ContractType', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'ContractDocumentDateAndNumber', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'DocumentDateAndNumber', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.UniversalTransferDocumentInfo
procedure DefineMessageFields_88;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.UniversalTransferDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 2);
    AddFieldDef(ftoOptional, ftInt32, nil, 'CurrencyCode', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.FunctionType'), 'Function', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalDocumentDateAndNumber', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfo.proto
// Diadoc.Api.Proto.Docflow.UniversalCorrectionDocumentInfo
procedure DefineMessageFields_89;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.UniversalCorrectionDocumentInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'TotalInc', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalDec', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'VatInc', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'VatDec', 4);
    AddFieldDef(ftoOptional, ftInt32, nil, 'CurrencyCode', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 6);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.FunctionType'), 'Function', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalDocumentDateAndNumber', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalDocumentRevisionDateAndNumber', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDateAndNumber'), 'OriginalDocumentCorrectionDateAndNumber', 10);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentSellerTitleInfo
procedure DefineMessageFields_90;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentSellerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.FunctionType'), 'Function', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentName', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 4);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Seller', 5);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Buyer', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Shipper'), 'Shipper', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Consignee', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 9);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo'), 'PaymentDocuments', 10);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTable'), 'InvoiceTable', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'Currency', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'CurrencyRate', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionDate', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionNumber', 15);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 16);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.TransferInfo'), 'TransferInfo', 17);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 18);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 19);
    AddFieldDef(ftoOptional, ftString, nil, 'GovernmentContractInfo', 20);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.Shipper
procedure DefineMessageFields_91;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Shipper')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'SameAsSeller', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'OrgInfo', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceTable
procedure DefineMessageFields_92;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTable')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.ExtendedInvoiceItem'), 'Items', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalNet', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.ExtendedInvoiceItem
procedure DefineMessageFields_93;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.ExtendedInvoiceItem')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Product', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Unit', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitName', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Quantity', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Excise', 6);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.TaxRate'), 'TaxRate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 9);
    AddFieldDef(ftoRequired, ftString, nil, 'Subtotal', 10);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CustomsDeclaration'), 'CustomsDeclarations', 11);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.ItemMark'), 'ItemMark', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalProperty', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemVendorCode', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemToRelease', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountDebit', 16);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountCredit', 17);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfo', 18);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.TransferInfo
procedure DefineMessageFields_94;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TransferInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'OperationInfo', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationType', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferDate', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.TransferBase'), 'TransferBase', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferTextInfo', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Waybill'), 'Waybill', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Carrier', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Employee'), 'Employee', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OtherIssuer'), 'OtherIssuer', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedThingTransferDate', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedThingInfo', 11);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 12);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.TransferBase
procedure DefineMessageFields_95;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TransferBase')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BaseDocumentName', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'BaseDocumentNumber', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'BaseDocumentDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'BaseDocumentInfo', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.Waybill
procedure DefineMessageFields_96;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Waybill')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'TransferDocumentNumber', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'TransferDocumentDate', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.Employee
procedure DefineMessageFields_97;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Employee')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'EmployeePosition', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'EmployeeInfo', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'EmployeeBase', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'TransferSurname', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'TransferFirstName', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferPatronymic', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.OtherIssuer
procedure DefineMessageFields_98;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.OtherIssuer')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'TransferEmployeePosition', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferEmployeeInfo', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferOrganizationName', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferOrganizationBase', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferEmployeeBase', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'TransferSurname', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'TransferFirstName', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferPatronymic', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.AdditionalInfoId
procedure DefineMessageFields_99;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'InfoFileId', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfo', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentBuyerTitleInfo
procedure DefineMessageFields_100;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentBuyerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationCode', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'OperationContent', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'AcceptanceDate', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Employee'), 'Employee', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OtherIssuer'), 'OtherIssuer', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.UniversalCorrectionDocumentSellerTitleInfo
procedure DefineMessageFields_101;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.UniversalCorrectionDocumentSellerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.FunctionType'), 'Function', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentName', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceForCorrectionInfo'), 'Invoices', 5);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Seller', 6);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Buyer', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 8);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.EventContent'), 'EventContent', 9);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceCorrectionTable'), 'InvoiceCorrectionTable', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'Currency', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'CurrencyRate', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'CorrectionRevisionDate', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'CorrectionRevisionNumber', 14);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 15);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 16);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 17);
    AddFieldDef(ftoOptional, ftString, nil, 'GovernmentContractInfo', 18);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceForCorrectionInfo
procedure DefineMessageFields_102;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceForCorrectionInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceDate', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceNumber', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceRevisionInfo'), 'InvoiceRevisions', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceRevisionInfo
procedure DefineMessageFields_103;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceRevisionInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceRevisionDate', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceRevisionNumber', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.EventContent
procedure DefineMessageFields_104;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.EventContent')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'CostChangeInfo', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferDocDetails', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'OperationContent', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'NotificationDate', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectionBase'), 'CorrectionBase', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.CorrectionBase
procedure DefineMessageFields_105;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectionBase')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BaseDocumentName', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'BaseDocumentNumber', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'BaseDocumentDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceCorrectionTable
procedure DefineMessageFields_106;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceCorrectionTable')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.ExtendedInvoiceCorrectionItem'), 'Items', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff'), 'TotalsInc', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff'), 'TotalsDec', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.ExtendedInvoiceCorrectionItem
procedure DefineMessageFields_107;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.ExtendedInvoiceCorrectionItem')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Product', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields'), 'OriginalValues', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields'), 'CorrectedValues', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff'), 'AmountsInc', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff'), 'AmountsDec', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountDebit', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountCredit', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfo', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceInfo
procedure DefineMessageFields_108;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceDate', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceNumber', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo'), 'Seller', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo'), 'Buyer', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.ShipperOrConsignee'), 'Shipper', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.ShipperOrConsignee'), 'Consignee', 6);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo'), 'PaymentDocuments', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItem'), 'Items', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Currency', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 12);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'InvoiceRevisionDate', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'InvoiceRevisionNumber', 16);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 17);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceFormatVersion'), 'Version', 18);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.AdditionalInfo
procedure DefineMessageFields_109;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Id', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Value', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceItem
procedure DefineMessageFields_110;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItem')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Product', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Unit', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Quantity', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 4);
    AddFieldDef(ftoRepeated, ftString, nil, 'CountriesOfOrigin', 5);
    AddFieldDef(ftoRepeated, ftString, nil, 'CustomsDeclarationNumbers', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'Excise', 7);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.TaxRate'), 'TaxRate', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'Subtotal', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 12);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CustomsDeclaration'), 'CustomsDeclarations', 13);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 14);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.CustomsDeclaration
procedure DefineMessageFields_111;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.CustomsDeclaration')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'CountryCode', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'DeclarationNumber', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo
procedure DefineMessageFields_112;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentNumber', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.ShipperOrConsignee
procedure DefineMessageFields_113;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.ShipperOrConsignee')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'SameAsSellerOrBuyer', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo'), 'OrgInfo', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceCorrectionInfo
procedure DefineMessageFields_114;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceCorrectionInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceDate', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceNumber', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'InvoiceRevisionDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'InvoiceRevisionNumber', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceCorrectionDate', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'InvoiceCorrectionNumber', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'InvoiceCorrectionRevisionDate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'InvoiceCorrectionRevisionNumber', 8);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo'), 'Seller', 9);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo'), 'Buyer', 10);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 11);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceCorrectionItem'), 'Items', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'Currency', 13);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff'), 'TotalsInc', 14);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff'), 'TotalsDec', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 16);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 17);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceFormatVersion'), 'Version', 18);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff
procedure DefineMessageFields_115;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceCorrectionItem
procedure DefineMessageFields_116;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceCorrectionItem')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Product', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields'), 'OriginalValues', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields'), 'CorrectedValues', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff'), 'AmountsInc', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff'), 'AmountsDec', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields
procedure DefineMessageFields_117;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Unit', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Quantity', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Excise', 4);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.TaxRate'), 'TaxRate', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'Subtotal', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff
procedure DefineMessageFields_118;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Excise', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Subtotal', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedOrganizationInfo.proto
// Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo
procedure DefineMessageFields_119;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'OrgName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'FnsParticipantId', 6);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.OrgType'), 'OrgType', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Okopf', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Okpo', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Okdp', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'Phone', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'Email', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'CorrespondentAccount', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'BankAccountNumber', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'BankName', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'BankId', 16);
    AddFieldDef(ftoOptional, ftString, nil, 'Department', 17);
    AddFieldDef(ftoOptional, ftString, nil, 'OrganizationAdditionalInfo', 18);
    AddFieldDef(ftoOptional, ftString, nil, 'OrganizationOrPersonInfo', 19);
    AddFieldDef(ftoOptional, ftString, nil, 'IndividualEntityRegistrationCertificate', 20);
    AddFieldDef(ftoOptional, ftString, nil, 'Country', 21);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\AsyncMethodResult.proto
// Diadoc.Api.Proto.AsyncMethodResult
procedure DefineMessageFields_120;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.AsyncMethodResult')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'TaskId', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CertificateInfo.proto
// Diadoc.Api.Proto.CertificateInfo
procedure DefineMessageFields_121;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CertificateInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Thumbprint', 1);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'ValidFrom', 2);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'ValidTo', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'OrganizationName', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Certificates\CertificateInfoV2.proto
// Diadoc.Api.Proto.Certificates.CertificateInfoV2
procedure DefineMessageFields_122;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Certificates.CertificateInfoV2')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Thumbprint', 1);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Certificates.CertificateType'), 'Type', 2);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'ValidFrom', 3);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'ValidTo', 4);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'PrivateKeyValidFrom', 5);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'PrivateKeyValidTo', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'OrganizationName', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Inn', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'UserFirstName', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'UserMiddleName', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'UserLastName', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'UserShortName', 12);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDefault', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Certificates\CertificateList.proto
// Diadoc.Api.Proto.Certificates.CertificateList
procedure DefineMessageFields_123;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Certificates.CertificateList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Certificates.CertificateInfoV2'), 'Certificates', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CloudSign.proto
// Diadoc.Api.Proto.CloudSignRequest
procedure DefineMessageFields_124;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CloudSignRequest')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CloudSignFile'), 'Files', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CloudSign.proto
// Diadoc.Api.Proto.CloudSignFile
procedure DefineMessageFields_125;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CloudSignFile')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Content_v2'), 'Content', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'FileName', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CloudSign.proto
// Diadoc.Api.Proto.CloudSignResult
procedure DefineMessageFields_126;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CloudSignResult')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Token', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CloudSign.proto
// Diadoc.Api.Proto.CloudSignConfirmResult
procedure DefineMessageFields_127;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CloudSignConfirmResult')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Content_v2'), 'Signatures', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CloudSign.proto
// Diadoc.Api.Proto.AutosignReceiptsResult
procedure DefineMessageFields_128;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.AutosignReceiptsResult')) do begin
    AddFieldDef(ftoRequired, ftInt64, nil, 'SignedReceiptsCount', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'NextBatchKey', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Content_v2.proto
// Diadoc.Api.Proto.Content_v2
procedure DefineMessageFields_129;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Content_v2')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'Content', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'NameOnShelf', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'PatchedContentId', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentIdEx'), 'DocumentId', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Content.proto
// Diadoc.Api.Proto.Content
procedure DefineMessageFields_130;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Content')) do begin
    AddFieldDef(ftoRequired, ftSfixed32, nil, 'Size', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'Data', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Content_v3.proto
// Diadoc.Api.Proto.Content_v3
procedure DefineMessageFields_131;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Content_v3')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'Content', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'NameOnShelf', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'EntityId', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'PatchedContentId', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Counteragent.proto
// Diadoc.Api.Proto.CounteragentList
procedure DefineMessageFields_132;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CounteragentList')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'TotalCount', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Counteragent'), 'Counteragents', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Counteragent.proto
// Diadoc.Api.Proto.Counteragent
procedure DefineMessageFields_133;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Counteragent')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'IndexKey', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Organization'), 'Organization', 2);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.CounteragentStatus'), 'CurrentStatus', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'LastEventTimestampTicks', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageFromCounteragent', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageToCounteragent', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InvitationDocumentId', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Counteragent.proto
// Diadoc.Api.Proto.CounteragentCertificateList
procedure DefineMessageFields_134;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CounteragentCertificateList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Certificate'), 'Certificates', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Counteragent.proto
// Diadoc.Api.Proto.Certificate
procedure DefineMessageFields_135;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Certificate')) do begin
    AddFieldDef(ftoRequired, ftBytes, nil, 'RawCertificateData', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
// Diadoc.Api.Proto.OrganizationList
procedure DefineMessageFields_136;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OrganizationList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Organization'), 'Organizations', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
// Diadoc.Api.Proto.Organization
procedure DefineMessageFields_137;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Organization')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'OrgId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Inn', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'FullName', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'ShortName', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Box'), 'Boxes', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Ogrn', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'FnsParticipantId', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'FnsRegistrationDate', 11);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Department'), 'Departments', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'IfnsCode', 13);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsPilot', 14);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsActive', 15);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsTest', 16);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsBranch', 17);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRoaming', 18);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsEmployee', 19);
    AddFieldDef(ftoOptional, ftInt32, nil, 'InvitationCount', 20);
    AddFieldDef(ftoOptional, ftInt32, nil, 'SearchCount', 21);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Sociability'), 'Sociability', 22);
    AddFieldDef(ftoOptional, ftString, nil, 'LiquidationDate', 23);
    AddFieldDef(ftoOptional, ftString, nil, 'CertificateOfRegistryInfo', 24);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsForeign', 25);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'HasCertificateToSign', 26);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
// Diadoc.Api.Proto.Department
procedure DefineMessageFields_138;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Department')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DepartmentId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'ParentDepartmentId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Abbreviation', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDisabled', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
// Diadoc.Api.Proto.Box
procedure DefineMessageFields_139;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Box')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'BoxIdGuid', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'Title', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Organization'), 'Organization', 3);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.OrganizationInvoiceFormatVersion'), 'InvoiceFormatVersion', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= 2;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'EncryptedDocumentsAllowed', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CustomPrintFormDetection.proto
// Diadoc.Api.Proto.CustomPrintFormDetectionRequest
procedure DefineMessageFields_140;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CustomPrintFormDetectionRequest')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentIds', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CustomPrintFormDetection.proto
// Diadoc.Api.Proto.CustomPrintFormDetectionResult
procedure DefineMessageFields_141;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CustomPrintFormDetectionResult')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomPrintFormDetectionItemResult'), 'Items', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\CustomPrintFormDetection.proto
// Diadoc.Api.Proto.CustomPrintFormDetectionItemResult
procedure DefineMessageFields_142;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CustomPrintFormDetectionItemResult')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'HasCustomPrintForm', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\Department.proto
// Diadoc.Api.Proto.Departments.Department
procedure DefineMessageFields_143;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.Department')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Id', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ParentDepartmentId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'Abbreviation', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 6);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.Routing'), 'Routing', 7);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'CreationTimestamp', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Timestamp.proto
// Diadoc.Api.Proto.Timestamp
procedure DefineMessageFields_144;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Timestamp')) do begin
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'Ticks', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\Routing.proto
// Diadoc.Api.Proto.Departments.Routing
procedure DefineMessageFields_145;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.Routing')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'Kpp', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'Address', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentList.proto
// Diadoc.Api.Proto.Departments.DepartmentList
procedure DefineMessageFields_146;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.Department'), 'Departments', 1);
    AddFieldDef(ftoRequired, ftInt32, nil, 'TotalCount', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToCreate.proto
// Diadoc.Api.Proto.Departments.DepartmentToCreate
procedure DefineMessageFields_147;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentToCreate')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ParentDepartmentId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Abbreviation', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 5);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.Routing'), 'Routing', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
// Diadoc.Api.Proto.Departments.DepartmentToUpdate
procedure DefineMessageFields_148;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentToUpdate')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.ParentDepartmentPatch'), 'ParentDepartment', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.DepartmentNamingPatch'), 'DepartmentNaming', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.DepartmentKppPatch'), 'Kpp', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.DepartmentAddressPatch'), 'Address', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Departments.DepartmentRoutingPatch'), 'Routing', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
// Diadoc.Api.Proto.Departments.ParentDepartmentPatch
procedure DefineMessageFields_149;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.ParentDepartmentPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentDepartmentId', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
// Diadoc.Api.Proto.Departments.DepartmentNamingPatch
procedure DefineMessageFields_150;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentNamingPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Abbreviation', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
// Diadoc.Api.Proto.Departments.DepartmentKppPatch
procedure DefineMessageFields_151;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentKppPatch')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Kpp', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
// Diadoc.Api.Proto.Departments.DepartmentAddressPatch
procedure DefineMessageFields_152;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentAddressPatch')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Departments\DepartmentToUpdate.proto
// Diadoc.Api.Proto.Departments.DepartmentRoutingPatch
procedure DefineMessageFields_153;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Departments.DepartmentRoutingPatch')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'Kpp', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'Address', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Attachment.proto
// Diadoc.Api.Proto.Docflow.Entity
procedure DefineMessageFields_154;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.Entity')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'EntityId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'CreationTimestamp', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Content'), 'Content', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Attachment.proto
// Diadoc.Api.Proto.Docflow.Attachment
procedure DefineMessageFields_155;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.Attachment')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Entity', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'AttachmentFilename', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'DisplayFilename', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Attachment.proto
// Diadoc.Api.Proto.Docflow.Signature
procedure DefineMessageFields_156;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.Signature')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Entity', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerBoxId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerDepartmentId', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsValid', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.SignatureVerificationResult'), 'VerificationResult', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Attachment.proto
// Diadoc.Api.Proto.Docflow.SignedAttachment
procedure DefineMessageFields_157;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Attachment'), 'Attachment', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Signature'), 'Signature', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Comment', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\SignatureVerificationResult.proto
// Diadoc.Api.Proto.SignatureVerificationResult
procedure DefineMessageFields_158;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.SignatureVerificationResult')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsValid', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.CertificateVerificationResult'), 'CertificateStatus', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SignatureTimestamp', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\SignatureVerificationResult.proto
// Diadoc.Api.Proto.CertificateVerificationResult
procedure DefineMessageFields_159;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CertificateVerificationResult')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsValid', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CertificateChainElement'), 'CertificateChain', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'VerificationTime', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\SignatureVerificationResult.proto
// Diadoc.Api.Proto.CertificateChainElement
procedure DefineMessageFields_160;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.CertificateChainElement')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'CertificateChainStatusFlags', 1);
    AddFieldDef(ftoRequired, ftBytes, nil, 'DerCertificate', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\AttachmentV3.proto
// Diadoc.Api.Proto.Docflow.SignatureV3
procedure DefineMessageFields_161;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SignatureV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Cms', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'CadesT', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'SignerBoxId', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'SignerDepartmentId', 4);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsValid', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.SignatureVerificationResult'), 'VerificationResult', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\AttachmentV3.proto
// Diadoc.Api.Proto.Docflow.SignedAttachmentV3
procedure DefineMessageFields_162;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Attachment'), 'Attachment', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureV3'), 'Signature', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Comment', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'ContentTypeId', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\BilateralDocflow.proto
// Diadoc.Api.Proto.Docflow.BilateralDocflow
procedure DefineMessageFields_163;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.BilateralDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow'), 'RecipientSignatureDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow'), 'RecipientSignatureRejectionDocflow', 4);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReceiptRequested', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRecipientSignatureRequested', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentSignedByRecipient', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentRejectedByRecipient', 8);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeReceipted', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedBySender', 10);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedOrRejectedByRecipient', 11);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ReceiptDocflow.proto
// Diadoc.Api.Proto.Docflow.ReceiptDocflow
procedure DefineMessageFields_164;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'ReceiptAttachment', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RecipientSignatureDocflow.proto
// Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow
procedure DefineMessageFields_165;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Signature'), 'RecipientSignature', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveryTimestamp', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RecipientSignatureRejectionDocflow.proto
// Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow
procedure DefineMessageFields_166;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'RecipientSignatureRejectionAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveryTimestamp', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Docflow.proto
// Diadoc.Api.Proto.Docflow.Docflow
procedure DefineMessageFields_167;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.Docflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'DocumentAttachment', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'DepartmentId', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DocumentIsDeleted', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatus'), 'DocflowStatus', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SendTimestamp', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveryTimestamp', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InboundInvoiceDocflow'), 'InboundInvoiceDocflow', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.OutboundInvoiceDocflow'), 'OutboundInvoiceDocflow', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.XmlBilateralDocflow'), 'XmlBilateralDocflow', 10);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.BilateralDocflow'), 'BilateralDocflow', 11);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.UnilateralDocflow'), 'UnilateralDocflow', 12);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RevocationDocflow'), 'RevocationDocflow', 13);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionDocflow'), 'ResolutionDocflow', 14);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeRevokedUnilaterallyBySender', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'PacketId', 16);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 17);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InboundUniversalTransferDocumentDocflow'), 'InboundUniversalTransferDocumentDocflow', 18);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.OutboundUniversalTransferDocumentDocflow'), 'OutboundUniversalTransferDocumentDocflow', 19);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RoamingNotification'), 'RoamingNotification', 20);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Docflow.proto
// Diadoc.Api.Proto.Docflow.DocflowStatus
procedure DefineMessageFields_168;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatus')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatusModel'), 'PrimaryStatus', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatusModel'), 'SecondaryStatus', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Docflow.proto
// Diadoc.Api.Proto.Docflow.DocflowStatusModel
procedure DefineMessageFields_169;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatusModel')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatusSeverity'), 'Severity', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'StatusText', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'StatusHint', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\InvoiceDocflow.proto
// Diadoc.Api.Proto.Docflow.InboundInvoiceDocflow
procedure DefineMessageFields_170;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InboundInvoiceDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow'), 'ConfirmationDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow'), 'CorrectionRequestDocflow', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ConfirmationTimestamp', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsAmendmentRequested', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRevised', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsCorrected', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\InvoiceDocflow.proto
// Diadoc.Api.Proto.Docflow.OutboundInvoiceDocflow
procedure DefineMessageFields_171;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.OutboundInvoiceDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow'), 'ConfirmationDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow'), 'CorrectionRequestDocflow', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ConfirmationTimestamp', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsAmendmentRequested', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRevised', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsCorrected', 8);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedBySender', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\InvoiceDocflow.proto
// Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow
procedure DefineMessageFields_172;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'ConfirmationAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\InvoiceDocflow.proto
// Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow
procedure DefineMessageFields_173;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'ReceiptAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow'), 'ConfirmationDocflow', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\InvoiceDocflow.proto
// Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow
procedure DefineMessageFields_174;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'CorrectionRequestAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\UnilateralDocflow.proto
// Diadoc.Api.Proto.Docflow.UnilateralDocflow
procedure DefineMessageFields_175;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.UnilateralDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReceiptRequested', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeReceipted', 4);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedBySender', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\XmlBilateralDocflow.proto
// Diadoc.Api.Proto.Docflow.XmlBilateralDocflow
procedure DefineMessageFields_176;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.XmlBilateralDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.BuyerTitleDocflow'), 'BuyerTitleDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow'), 'RecipientSignatureRejectionDocflow', 4);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReceiptRequested', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentSignedByRecipient', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentRejectedByRecipient', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeReceipted', 8);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedBySender', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedOrRejectedByRecipient', 10);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\XmlBilateralDocflow.proto
// Diadoc.Api.Proto.Docflow.BuyerTitleDocflow
procedure DefineMessageFields_177;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.BuyerTitleDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'BuyerTitleAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SendTimestamp', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveryTimestamp', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RevocationDocflow.proto
// Diadoc.Api.Proto.Docflow.RevocationDocflow
procedure DefineMessageFields_178;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RevocationDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachment'), 'RevocationRequestAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow'), 'RecipientSignatureDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow'), 'RecipientSignatureRejectionDocflow', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'InitiatorBoxId', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRevocationAccepted', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRevocationRejected', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflow.proto
// Diadoc.Api.Proto.Docflow.ResolutionDocflow
procedure DefineMessageFields_179;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionDocflow')) do begin
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\UniversalTransferDocumentDocflow.proto
// Diadoc.Api.Proto.Docflow.InboundUniversalTransferDocumentDocflow
procedure DefineMessageFields_180;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.InboundUniversalTransferDocumentDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow'), 'ConfirmationDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow'), 'CorrectionRequestDocflow', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ConfirmationTimestamp', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsAmendmentRequested', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRevised', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsCorrected', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.BuyerTitleDocflow'), 'BuyerTitleDocflow', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow'), 'RecipientSignatureRejectionDocflow', 10);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReceiptRequested', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRecipientSignatureRequested', 12);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentSignedByRecipient', 13);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentRejectedByRecipient', 14);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeReceipted', 15);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedOrRejectedByRecipient', 16);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\UniversalTransferDocumentDocflow.proto
// Diadoc.Api.Proto.Docflow.OutboundUniversalTransferDocumentDocflow
procedure DefineMessageFields_181;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.OutboundUniversalTransferDocumentDocflow')) do begin
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflow'), 'ReceiptDocflow', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow'), 'ConfirmationDocflow', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow'), 'CorrectionRequestDocflow', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ConfirmationTimestamp', 5);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsAmendmentRequested', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRevised', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsCorrected', 8);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedBySender', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.BuyerTitleDocflow'), 'BuyerTitleDocflow', 10);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow'), 'RecipientSignatureRejectionDocflow', 11);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReceiptRequested', 12);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRecipientSignatureRequested', 13);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentSignedByRecipient', 14);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDocumentRejectedByRecipient', 15);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeReceipted', 16);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'CanDocumentBeSignedOrRejectedByRecipient', 17);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\RoamingNotification.proto
// Diadoc.Api.Proto.Docflow.RoamingNotification
procedure DefineMessageFields_182;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RoamingNotification')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Notification', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsSuccess', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowBatchRequest
procedure DefineMessageFields_183;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowBatchRequest')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowRequest'), 'Requests', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowRequest
procedure DefineMessageFields_184;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowRequest')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'LastEventId', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'InjectEntityContent', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowBatchResponse
procedure DefineMessageFields_185;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowBatchResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow'), 'Documents', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.SearchDocflowsRequest
procedure DefineMessageFields_186;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SearchDocflowsRequest')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'QueryString', 1);
    AddFieldDef(ftoOptional, ftInt32, nil, 'Count', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 100;
    AddFieldDef(ftoOptional, ftInt32, nil, 'FirstIndex', 3);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Docflow.SearchScope'), 'Scope', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'InjectEntityContent', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.SearchDocflowsResponse
procedure DefineMessageFields_187;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SearchDocflowsResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow'), 'Documents', 1);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'HaveMoreDocuments', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdRequest
procedure DefineMessageFields_188;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdRequest')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'PacketId', 1);
    AddFieldDef(ftoOptional, ftInt32, nil, 'Count', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 100;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'InjectEntityContent', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBytes, nil, 'AfterIndexKey', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.FetchedDocument
procedure DefineMessageFields_189;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.FetchedDocument')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow'), 'Document', 1);
    AddFieldDef(ftoRequired, ftBytes, nil, 'IndexKey', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponse
procedure DefineMessageFields_190;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.FetchedDocument'), 'Documents', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'NextPageIndexKey', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowEventsRequest
procedure DefineMessageFields_191;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowEventsRequest')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.TimeBasedFilter'), 'Filter', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'AfterIndexKey', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'PopulateDocuments', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'InjectEntityContent', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'PopulatePreviousDocumentStates', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.GetDocflowEventsResponse
procedure DefineMessageFields_192;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowEventsResponse')) do begin
    AddFieldDef(ftoOptional, ftInt32, nil, 'TotalCount', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowEvent'), 'Events', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.TotalCountType'), 'TotalCountType', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.DocflowEvent
procedure DefineMessageFields_193;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocflowEvent')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'EventId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'Timestamp', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 3);
    AddFieldDef(ftoOptional, ftBytes, nil, 'IndexKey', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow'), 'Document', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'PreviousEventId', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow'), 'PreviousDocumentState', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\TimeBasedFilter.proto
// Diadoc.Api.Proto.TimeBasedFilter
procedure DefineMessageFields_194;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.TimeBasedFilter')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'FromTimestamp', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ToTimestamp', 2);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.SortDirection'), 'SortDirection', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= 1;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentWithDocflow.proto
// Diadoc.Api.Proto.Docflow.DocumentWithDocflow
procedure DefineMessageFields_195;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'LastEventId', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'LastEventTimestamp', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentInfo'), 'DocumentInfo', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Docflow'), 'Docflow', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.ForwardDocumentEvent'), 'ForwardDocumentEvents', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ForwardDocumentEvent.proto
// Diadoc.Api.Proto.ForwardDocumentEvent
procedure DefineMessageFields_196;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.ForwardDocumentEvent')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'Timestamp', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ToBoxId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
// Diadoc.Api.Proto.Docflow.GetDocflowBatchResponseV3
procedure DefineMessageFields_197;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowBatchResponseV3')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3'), 'Documents', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
// Diadoc.Api.Proto.Docflow.SearchDocflowsResponseV3
procedure DefineMessageFields_198;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SearchDocflowsResponseV3')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3'), 'Documents', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'HaveMoreDocuments', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
// Diadoc.Api.Proto.Docflow.FetchedDocumentV3
procedure DefineMessageFields_199;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.FetchedDocumentV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3'), 'Document', 1);
    AddFieldDef(ftoRequired, ftBytes, nil, 'IndexKey', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
// Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponseV3
procedure DefineMessageFields_200;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponseV3')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.FetchedDocumentV3'), 'Documents', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'NextPageIndexKey', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
// Diadoc.Api.Proto.Docflow.GetDocflowEventsResponseV3
procedure DefineMessageFields_201;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.GetDocflowEventsResponseV3')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'TotalCount', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowEventV3'), 'Events', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.TotalCountType'), 'TotalCountType', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApiV3.proto
// Diadoc.Api.Proto.Docflow.DocflowEventV3
procedure DefineMessageFields_202;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocflowEventV3')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'EventId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'Timestamp', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 3);
    AddFieldDef(ftoRequired, ftBytes, nil, 'IndexKey', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3'), 'Document', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'PreviousEventId', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3'), 'PreviousDocumentState', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentWithDocflowV3.proto
// Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3
procedure DefineMessageFields_203;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.LastEvent'), 'LastEvent', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentInfoV3'), 'DocumentInfo', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocflowV3'), 'Docflow', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentWithDocflowV3.proto
// Diadoc.Api.Proto.Docflow.LastEvent
procedure DefineMessageFields_204;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.LastEvent')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'EventId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'Timestamp', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentInfoV3
procedure DefineMessageFields_205;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentInfoV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.FullVersion'), 'FullVersion', 1);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.MessageType'), 'MessageType', 2);
    AddFieldDef(ftoRequired, ftInt32, nil, 'WorkflowId', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipants'), 'Participants', 4);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentDirection'), 'DocumentDirection', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'DepartmentId', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.MetadataItem'), 'Metadata', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 9);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentLinks'), 'DocumentLinks', 10);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.PacketInfo'), 'PacketInfo', 11);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsRead', 12);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsDeleted', 13);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsInvitation', 14);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentLetterInfo'), 'LetterInfo', 15);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDraftInfo'), 'DraftInfo', 16);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentTemplateInfo'), 'TemplateInfo', 17);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Origin'), 'Origin', 18);
    AddFieldDef(ftoOptional, ftString, nil, 'EditingSettingId', 19);
    FieldDef[FieldDefsCount-1].DefaultValue:= '';
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentParticipants
procedure DefineMessageFields_206;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipants')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipant'), 'Sender', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipant'), 'Proxy', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipant'), 'Recipient', 3);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsInternal', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentParticipant
procedure DefineMessageFields_207;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipant')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DepartmentId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentLinks
procedure DefineMessageFields_208;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentLinks')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialIds', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateIds', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.PacketInfo
procedure DefineMessageFields_209;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.PacketInfo')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.LockMode'), 'LockMode', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'PacketId', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'AddedAt', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentLetterInfo
procedure DefineMessageFields_210;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentLetterInfo')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsEncrypted', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.ForwardDocumentEvent'), 'ForwardDocumentEvents', 2);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsTest', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentDraftInfo
procedure DefineMessageFields_211;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentDraftInfo')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsRecycled', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsLocked', 2);
    AddFieldDef(ftoRepeated, ftString, nil, 'TransformedToLetterIds', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.DocumentTemplateInfo
procedure DefineMessageFields_212;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocumentTemplateInfo')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentParticipants'), 'LetterParticipants', 1);
    AddFieldDef(ftoRepeated, ftString, nil, 'TransformedToLetterIds', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.TemplateTransformationInfo'), 'TemplateTransformationInfos', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.TemplateRefusalInfo'), 'TemplateRefusalInfo', 4);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReusable', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.TemplateTransformationInfo
procedure DefineMessageFields_213;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.TemplateTransformationInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'TransformationId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'TransformedToDocumentId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'AuthorUserId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocumentInfoV3.proto
// Diadoc.Api.Proto.Docflow.TemplateRefusalInfo
procedure DefineMessageFields_214;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.TemplateRefusalInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'AuthorUserId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\FullVersion.proto
// Diadoc.Api.Proto.FullVersion
procedure DefineMessageFields_215;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.FullVersion')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Function', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.Document
procedure DefineMessageFields_216;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Document')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'IndexKey', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'EntityId', 3);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'CreationTimestampTicks', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'CounteragentBoxId', 5);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentType'), 'DocumentType', 6);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InitialDocumentIds', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'SubordinateDocumentIds', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Content'), 'Content', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'FileName', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentDate', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 12);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata'), 'NonformalizedDocumentMetadata', 13);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceMetadata'), 'InvoiceMetadata', 14);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.TrustConnectionRequestMetadata'), 'TrustConnectionRequestMetadata', 15);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata'), 'Torg12Metadata', 16);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceRevisionMetadata'), 'InvoiceRevisionMetadata', 17);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionMetadata'), 'InvoiceCorrectionMetadata', 18);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionRevisionMetadata'), 'InvoiceCorrectionRevisionMetadata', 19);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateMetadata'), 'AcceptanceCertificateMetadata', 20);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.ProformaInvoiceMetadata'), 'ProformaInvoiceMetadata', 21);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata'), 'XmlTorg12Metadata', 22);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata'), 'XmlAcceptanceCertificateMetadata', 23);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDeleted', 24);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'DepartmentId', 25);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsTest', 26);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'FromDepartmentId', 27);
    AddFieldDef(ftoOptional, ftString, nil, 'ToDepartmentId', 28);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.PriceListMetadata'), 'PriceListMetadata', 29);
    AddFieldDef(ftoOptional, ftString, nil, 'CustomDocumentId', 30);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.ResolutionStatus'), 'ResolutionStatus', 31);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.RevocationStatus'), 'RevocationStatus', 32);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'SendTimestampTicks', 33);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'DeliveryTimestampTicks', 34);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.ForwardDocumentEvent'), 'ForwardDocumentEvents', 35);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentMetadata'), 'ReconciliationActMetadata', 38);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.ContractMetadata'), 'ContractMetadata', 39);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata'), 'Torg13Metadata', 40);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.ServiceDetailsMetadata'), 'ServiceDetailsMetadata', 41);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.RoamingNotificationStatus'), 'RoamingNotificationStatus', 42);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'HasCustomPrintForm', 43);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CustomDataItem'), 'CustomData', 44);
    AddFieldDef(ftoOptional, ftString, nil, 'PacketId', 45);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentDirection'), 'DocumentDirection', 46);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'LastModificationTimestampTicks', 47);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsEncryptedContent', 48);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.SenderSignatureStatus'), 'SenderSignatureStatus', 49);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.SupplementaryAgreementMetadata'), 'SupplementaryAgreementMetadata', 50);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsRead', 51);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'RoamingNotificationStatusDescription', 52);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'PacketIsLocked', 53);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata'), 'PriceListAgreementMetadata', 54);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata'), 'CertificateRegistryMetadata', 55);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentMetadata'), 'UniversalTransferDocumentMetadata', 56);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentRevisionMetadata'), 'UniversalTransferDocumentRevisionMetadata', 57);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentMetadata'), 'UniversalCorrectionDocumentMetadata', 58);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentRevisionMetadata'), 'UniversalCorrectionDocumentRevisionMetadata', 59);
    AddFieldDef(ftoOptional, ftString, nil, 'ResolutionRouteId', 60);
    FieldDef[FieldDefsCount-1].DefaultValue:= '';
    AddFieldDef(ftoOptional, ftString, nil, 'AttachmentVersion', 61);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ProxySignatureStatus'), 'ProxySignatureStatus', 62);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 63);
    AddFieldDef(ftoRequired, ftString, nil, 'Function', 64);
    AddFieldDef(ftoRequired, ftInt32, nil, 'WorkflowId', 65);
    AddFieldDef(ftoRequired, ftString, nil, 'Title', 66);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.MetadataItem'), 'Metadata', 67);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.RecipientReceiptMetadata'), 'RecipientReceiptMetadata', 68);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.ConfirmationMetadata'), 'ConfirmationMetadata', 69);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.RecipientResponseStatus'), 'RecipientResponseStatus', 70);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.AmendmentRequestMetadata'), 'AmendmentRequestMetadata', 71);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Origin'), 'Origin', 72);
    AddFieldDef(ftoOptional, ftString, nil, 'EditingSettingId', 73);
    FieldDef[FieldDefsCount-1].DefaultValue:= '';
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.LockMode'), 'LockMode', 74);
    FieldDef[FieldDefsCount-1].DefaultValue:= 1;
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.SenderReceiptMetadata'), 'SenderReceiptMetadata', 75);
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 76);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.LastOuterDocflow'), 'LastOuterDocflows', 77);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyBoxId', 78);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyDepartmentId', 79);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.LastOuterDocflow
procedure DefineMessageFields_217;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.LastOuterDocflow')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.OuterDocflowInfo'), 'OuterDocflow', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.ResolutionStatus
procedure DefineMessageFields_218;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.ResolutionStatus')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ResolutionStatusType'), 'Type', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.ResolutionTarget'), 'Target', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'AuthorUserId', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'AuthorFIO', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.RecipientReceiptMetadata
procedure DefineMessageFields_219;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.RecipientReceiptMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.GeneralReceiptStatus'), 'ReceiptStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.ConfirmationMetadata'), 'ConfirmationMetadata', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.SenderReceiptMetadata
procedure DefineMessageFields_220;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.SenderReceiptMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.GeneralReceiptStatus'), 'ReceiptStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.ConfirmationMetadata
procedure DefineMessageFields_221;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.ConfirmationMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.GeneralReceiptStatus'), 'ReceiptStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'DateTimeTicks', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.AmendmentRequestMetadata
procedure DefineMessageFields_222;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.AmendmentRequestMetadata')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'AmendmentFlags', 1);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.GeneralReceiptStatus'), 'ReceiptStatus', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.Origin
procedure DefineMessageFields_223;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Origin')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.MessageType'), 'MessageType', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\AcceptanceCertificateDocument.proto
// Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateMetadata
procedure DefineMessageFields_224;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.TrustConnectionRequestMetadata
procedure DefineMessageFields_225;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.TrustConnectionRequestMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus'), 'TrustConnectionRequestStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata
procedure DefineMessageFields_226;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionDate', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionNumber', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.PriceListMetadata
procedure DefineMessageFields_227;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.PriceListMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'PriceListEffectiveDate', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractDocumentDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractDocumentNumber', 4);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.ContractMetadata
procedure DefineMessageFields_228;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.ContractMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'ContractPrice', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractType', 3);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.SupplementaryAgreementMetadata
procedure DefineMessageFields_229;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.SupplementaryAgreementMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ContractType', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'ContractNumber', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'ContractDate', 5);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 6);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentMetadata
procedure DefineMessageFields_230;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\InvoiceDocument.proto
// Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceMetadata
procedure DefineMessageFields_231;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus'), 'InvoiceStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 4);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 5);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\InvoiceDocument.proto
// Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceRevisionMetadata
procedure DefineMessageFields_232;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceRevisionMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus'), 'InvoiceRevisionStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceNumber', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceDate', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'Vat', 5);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 6);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 7);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 8);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\InvoiceDocument.proto
// Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionMetadata
procedure DefineMessageFields_233;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus'), 'InvoiceCorrectionStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceNumber', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionNumber', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionDate', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalInc', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalDec', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'VatInc', 8);
    AddFieldDef(ftoRequired, ftString, nil, 'VatDec', 9);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 10);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 11);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 12);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\InvoiceDocument.proto
// Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionRevisionMetadata
procedure DefineMessageFields_234;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionRevisionMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus'), 'InvoiceCorrectionRevisionStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceNumber', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionNumber', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionDate', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceCorrectionNumber', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceCorrectionDate', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalInc', 8);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalDec', 9);
    AddFieldDef(ftoRequired, ftString, nil, 'VatInc', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'VatDec', 11);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 12);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 13);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 14);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\NonformalizedDocument.proto
// Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata
procedure DefineMessageFields_235;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UnilateralDocument.proto
// Diadoc.Api.Proto.Documents.UnilateralDocument.ProformaInvoiceMetadata
procedure DefineMessageFields_236;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.ProformaInvoiceMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.UnilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UnilateralDocument.proto
// Diadoc.Api.Proto.Documents.UnilateralDocument.ServiceDetailsMetadata
procedure DefineMessageFields_237;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.ServiceDetailsMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.UnilateralDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus'), 'ReceiptStatus', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UniversalTransferDocument.proto
// Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentMetadata
procedure DefineMessageFields_238;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentMetadata')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus'), 'DocumentStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentFunction', 5);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 6);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 8);
    AddFieldDef(ftoOptional, ftInt32, nil, 'InvoiceAmendmentFlags', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UniversalTransferDocument.proto
// Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentRevisionMetadata
procedure DefineMessageFields_239;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentRevisionMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus'), 'DocumentStatus', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentFunction', 5);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 6);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 7);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 8);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceNumber', 9);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceDate', 10);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UniversalTransferDocument.proto
// Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentMetadata
procedure DefineMessageFields_240;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus'), 'DocumentStatus', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalInc', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalDec', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'VatInc', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'VatDec', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentFunction', 7);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 8);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 9);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceNumber', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceDate', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionNumber', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionDate', 14);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UniversalTransferDocument.proto
// Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentRevisionMetadata
procedure DefineMessageFields_241;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentRevisionMetadata')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus'), 'DocumentStatus', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalInc', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'TotalDec', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'VatInc', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'VatDec', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Grounds', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentFunction', 7);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Currency', 8);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'ConfirmationDateTimeTicks', 9);
    AddFieldDef(ftoRequired, ftInt32, nil, 'InvoiceAmendmentFlags', 10);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceNumber', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceDate', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionNumber', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'OriginalInvoiceRevisionDate', 14);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceCorrectionNumber', 15);
    AddFieldDef(ftoRequired, ftString, nil, 'OriginalInvoiceCorrectionDate', 16);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OuterDocflow.proto
// Diadoc.Api.Proto.OuterDocflowInfo
procedure DefineMessageFields_242;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OuterDocflowInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocflowNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'DocflowFriendlyName', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Status'), 'Status', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OuterDocflowStatus.proto
// Diadoc.Api.Proto.Status
procedure DefineMessageFields_243;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Status')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'NamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FriendlyName', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.OuterStatusType'), 'Type', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Description', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.StatusDetail'), 'Details', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OuterDocflowStatus.proto
// Diadoc.Api.Proto.StatusDetail
procedure DefineMessageFields_244;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.StatusDetail')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Code', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Text', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.DocflowV3
procedure DefineMessageFields_245;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.DocflowV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SenderTitleDocflow'), 'SenderTitle', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ConfirmationDocflow'), 'Confirmation', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow'), 'ProxyResponse', 11);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflowV3'), 'RecipientReceipt', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow'), 'RecipientResponse', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.AmendmentRequestDocflow'), 'AmendmentRequest', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RevocationDocflowV3'), 'Revocation', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflowV3'), 'SenderReceipt', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionDocflowV3'), 'Resolution', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3'), 'ResolutionEntities', 10);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.OuterDocflow'), 'OuterDocflows', 12);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.OuterDocflowEntities'), 'OuterDocflowEntities', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.SenderTitleDocflow
procedure DefineMessageFields_246;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SenderTitleDocflow')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'Attachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SentAt', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RoamingNotification'), 'RoamingNotification', 5);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.SenderSignatureStatus'), 'SenderSignatureStatus', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.ConfirmationDocflow
procedure DefineMessageFields_247;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ConfirmationDocflow')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'ConfirmationAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ConfirmedAt', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflowV3'), 'Receipt', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow
procedure DefineMessageFields_248;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'SignatureRejection', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFormal', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'PlainText', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow
procedure DefineMessageFields_249;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureV3'), 'Signature', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'Title', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow'), 'Rejection', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SentAt', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 6);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.RecipientResponseStatus'), 'ResponseStatus', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.AmendmentRequestDocflow
procedure DefineMessageFields_250;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.AmendmentRequestDocflow')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'AmendmentRequest', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SentAt', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflowV3'), 'Receipt', 5);
    AddFieldDef(ftoRequired, ftInt32, nil, 'AmendmentFlags', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'PlainText', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.RevocationDocflowV3
procedure DefineMessageFields_251;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RevocationDocflowV3')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RevocationRequestDocflow'), 'RevocationRequest', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RevocationResponseDocflow'), 'RevocationResponse', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'InitiatorBoxId', 4);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.RevocationStatus'), 'RevocationStatus', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3'), 'ResolutionEntities', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.OuterDocflowEntities'), 'OuterDocflowEntities', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.RevocationRequestDocflow
procedure DefineMessageFields_252;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RevocationRequestDocflow')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'RevocationRequest', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SentAt', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.RoamingNotification'), 'RoamingNotification', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'PlainText', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.RevocationResponseDocflow
procedure DefineMessageFields_253;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.RevocationResponseDocflow')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureV3'), 'RecipientSignature', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow'), 'SignatureRejection', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.ReceiptDocflowV3
procedure DefineMessageFields_254;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ReceiptDocflowV3')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'ReceiptAttachment', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SentAt', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'DeliveredAt', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ConfirmationDocflow'), 'Confirmation', 5);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.GeneralReceiptStatus'), 'Status', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.OuterDocflow
procedure DefineMessageFields_255;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.OuterDocflow')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocflowNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'OuterDocflowEntityId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.OuterDocflowEntities
procedure DefineMessageFields_256;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.OuterDocflowEntities')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocflowNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'DocflowFriendlyName', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.StatusEntity'), 'StatusEntities', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowV3.proto
// Diadoc.Api.Proto.Docflow.StatusEntity
procedure DefineMessageFields_257;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.StatusEntity')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignedAttachmentV3'), 'Attachment', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Status'), 'Status', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.ResolutionDocflowV3
procedure DefineMessageFields_258;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionDocflowV3')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFinished', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'ParentEntityId', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionStatus'), 'ResolutionStatus', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'ResolutionEntityId', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3
procedure DefineMessageFields_259;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionRequestV3'), 'Requests', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionV3'), 'Resolutions', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.ApprovementSignatureV3'), 'ApprovementSignatures', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureDenialV3'), 'SignatureDenials', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.ResolutionRequestV3
procedure DefineMessageFields_260;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionRequestV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Entity', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.ResolutionTarget'), 'Target', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'AuthorUserId', 3);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionRequestType'), 'RequestType', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoOptional, ftString, nil, 'ResolvedWith', 5);
    AddFieldDef(ftoRepeated, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionAction'), 'Actions', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.ResolutionV3
procedure DefineMessageFields_261;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Entity', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ResolutionRequestId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'AuthorUserId', 3);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.ResolutionType'), 'ResolutionType', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.ApprovementSignatureV3
procedure DefineMessageFields_262;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.ApprovementSignatureV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.SignatureV3'), 'Signature', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ResolutionRequestId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'AuthorUserId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.SignatureDenialV3
procedure DefineMessageFields_263;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Docflow.SignatureDenialV3')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.Entity'), 'Entity', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'ResolutionRequestId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'AuthorUserId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DetectTitleResponse.proto
// Diadoc.Api.Proto.Documents.DetectTitleResponse
procedure DefineMessageFields_264;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.DetectTitleResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.DetectedDocumentTitle'), 'DocumentTitles', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DetectTitleResponse.proto
// Diadoc.Api.Proto.Documents.DetectedDocumentTitle
procedure DefineMessageFields_265;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.DetectedDocumentTitle')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Function', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 3);
    AddFieldDef(ftoRequired, ftInt32, nil, 'TitleIndex', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.MetadataItem'), 'Metadata', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentList.proto
// Diadoc.Api.Proto.Documents.DocumentList
procedure DefineMessageFields_266;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.DocumentList')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'TotalCount', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Document'), 'Documents', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'HasMoreResults', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentProtocol.proto
// Diadoc.Api.Proto.Documents.DocumentProtocol
procedure DefineMessageFields_267;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.DocumentProtocol')) do begin
    AddFieldDef(ftoRequired, ftBytes, nil, 'PrintForm', 1);
    AddFieldDef(ftoRequired, ftBytes, nil, 'Signature', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentsMoveOperation.proto
// Diadoc.Api.Proto.Documents.DocumentsMoveOperation
procedure DefineMessageFields_268;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.DocumentsMoveOperation')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ToDepartmentId', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentIds', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\DocumentZip.proto
// Diadoc.Api.Proto.Documents.DocumentZipGenerationResult
procedure DefineMessageFields_269;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.DocumentZipGenerationResult')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ZipFileNameOnShelf', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescription.proto
// Diadoc.Api.Proto.Documents.Types.DetectedDocumentType
procedure DefineMessageFields_270;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DetectedDocumentType')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'TypeNamedId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Function', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescription.proto
// Diadoc.Api.Proto.Documents.Types.DetectDocumentTypesResponse
procedure DefineMessageFields_271;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DetectDocumentTypesResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DetectedDocumentType'), 'DocumentTypes', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.DocumentTypeDescriptionV2
procedure DefineMessageFields_272;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentTypeDescriptionV2')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Title', 2);
    AddFieldDef(ftoRepeated, ftInt32, nil, 'SupportedDocflows', 3);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'RequiresFnsRegistration', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentFunctionV2'), 'Functions', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.GetDocumentTypesResponseV2
procedure DefineMessageFields_273;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.GetDocumentTypesResponseV2')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentTypeDescriptionV2'), 'DocumentTypes', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.DocumentFunctionV2
procedure DefineMessageFields_274;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentFunctionV2')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentVersionV2'), 'Versions', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.DocumentVersionV2
procedure DefineMessageFields_275;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentVersionV2')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Version', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'SupportsContentPatching', 2);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'SupportsEncrypting', 3);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'SupportsPredefinedRecipientTitle', 7);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'SupportsAmendmentRequest', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentTitleV2'), 'Titles', 4);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsActual', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentWorkflowV2'), 'Workflows', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.DocumentWorkflowV2
procedure DefineMessageFields_276;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentWorkflowV2')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'Id', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsDefault', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.DocumentTitleV2
procedure DefineMessageFields_277;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentTitleV2')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'Index', 7);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsFormal', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'XsdUrl', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'UserDataXsdUrl', 5);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.SignerInfoV2'), 'SignerInfo', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2'), 'MetadataItems', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2'), 'EncryptedMetadataItems', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.SignerInfoV2
procedure DefineMessageFields_278;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.SignerInfoV2')) do begin
    AddFieldDef(ftoRequired, ftInt32, nil, 'SignerType', 1);
    AddFieldDef(ftoRequired, ftInt32, nil, 'ExtendedDocumentTitleType', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Types\DocumentTypeDescriptionV2.proto
// Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2
procedure DefineMessageFields_279;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Id', 1);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Type', 2);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsRequired', 3);
    AddFieldDef(ftoRequired, ftInt32, nil, 'Source', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssSignRequest
procedure DefineMessageFields_280;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Dss.DssSignRequest')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Dss.DssSignFile'), 'Files', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssSignFile
procedure DefineMessageFields_281;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Dss.DssSignFile')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Content_v3'), 'Content', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'FileName', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssSignResult
procedure DefineMessageFields_282;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Dss.DssSignResult')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Dss.DssOperationStatus'), 'OperationStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Dss.DssFileSigningResult'), 'FileSigningResults', 2);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Dss.DssConfirmType'), 'ConfirmType', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Dss.DssOperator'), 'Operator', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftString, nil, 'PhoneLastNumbers', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssFileSigningResult
procedure DefineMessageFields_283;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Dss.DssFileSigningResult')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Dss.DssFileSigningStatus'), 'FileSigningStatus', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftBytes, nil, 'Signature', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Employee.proto
// Diadoc.Api.Proto.Employees.Employee
procedure DefineMessageFields_284;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.Employee')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.UserV2'), 'User', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeePermissions'), 'Permissions', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'Position', 3);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanBeInvitedForChat', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'CreationTimestamp', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Employee.proto
// Diadoc.Api.Proto.Employees.EmployeePermissions
procedure DefineMessageFields_285;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeePermissions')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'UserDepartmentId', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsAdministrator', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentAccessLevel'), 'DocumentAccessLevel', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoRepeated, ftString, nil, 'SelectedDepartmentIds', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeAction'), 'Actions', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.AuthorizationPermission'), 'AuthorizationPermission', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Employee.proto
// Diadoc.Api.Proto.Employees.EmployeeAction
procedure DefineMessageFields_286;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeAction')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsAllowed', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Employee.proto
// Diadoc.Api.Proto.Employees.EmployeeList
procedure DefineMessageFields_287;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.Employee'), 'Employees', 1);
    AddFieldDef(ftoRequired, ftInt32, nil, 'TotalCount', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\User.proto
// Diadoc.Api.Proto.User
procedure DefineMessageFields_288;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.User')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Id', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'LastName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'FirstName', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'MiddleName', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.CertificateInfo'), 'CloudCertificates', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\User.proto
// Diadoc.Api.Proto.UserV2
procedure DefineMessageFields_289;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.UserV2')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'UserId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Login', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.FullName'), 'FullName', 3);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsRegistered', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\User.proto
// Diadoc.Api.Proto.FullName
procedure DefineMessageFields_290;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.FullName')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'LastName', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FirstName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'MiddleName', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUserPermissions.proto
// Diadoc.Api.Proto.OrganizationUserPermissions
procedure DefineMessageFields_291;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OrganizationUserPermissions')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'UserDepartmentId', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsAdministrator', 2);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentAccessLevel'), 'DocumentAccessLevel', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanSignDocuments', 4);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanManageCounteragents', 6);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanAddResolutions', 7);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanRequestResolutions', 8);
    AddFieldDef(ftoRepeated, ftString, nil, 'SelectedDepartmentIds', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'JobTitle', 10);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanCreateDocuments', 11);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.AuthorizationPermission'), 'AuthorizationPermission', 12);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanDeleteRestoreDocuments', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUserPermissions.proto
// Diadoc.Api.Proto.AuthorizationPermission
procedure DefineMessageFields_292;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.AuthorizationPermission')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsBlocked', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToCreate.proto
// Diadoc.Api.Proto.Employees.EmployeeToCreate
procedure DefineMessageFields_293;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreate')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreateCredentials'), 'Credentials', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Position', 2);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanBeInvitedForChat', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeePermissions'), 'Permissions', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToCreate.proto
// Diadoc.Api.Proto.Employees.EmployeeToCreateCredentials
procedure DefineMessageFields_294;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreateCredentials')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreateByLogin'), 'Login', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreateByCertificate'), 'Certificate', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToCreate.proto
// Diadoc.Api.Proto.Employees.EmployeeToCreateByLogin
procedure DefineMessageFields_295;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreateByLogin')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Login', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.FullName'), 'FullName', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToCreate.proto
// Diadoc.Api.Proto.Employees.EmployeeToCreateByCertificate
procedure DefineMessageFields_296;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToCreateByCertificate')) do begin
    AddFieldDef(ftoRequired, ftBytes, nil, 'Content', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'AccessBasis', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Email', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeeToUpdate
procedure DefineMessageFields_297;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeToUpdate')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeePermissionsPatch'), 'Permissions', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeePositionPatch'), 'Position', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeCanBeInvitedForChatPatch'), 'CanBeInvitedForChat', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeePermissionsPatch
procedure DefineMessageFields_298;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeePermissionsPatch')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeDepartmentPatch'), 'Department', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeIsAdministratorPatch'), 'IsAdministrator', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeDocumentAccessLevelPatch'), 'DocumentAccessLevel', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeSelectedDepartmentsPatch'), 'SelectedDepartments', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.EmployeeAction'), 'Actions', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.AuthorizationPermissionPatch'), 'AuthorizationPermission', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeeDepartmentPatch
procedure DefineMessageFields_299;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeDepartmentPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DepartmentId', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeeIsAdministratorPatch
procedure DefineMessageFields_300;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeIsAdministratorPatch')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsAdministrator', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeeDocumentAccessLevelPatch
procedure DefineMessageFields_301;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeDocumentAccessLevelPatch')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.DocumentAccessLevel'), 'DocumentAccessLevel', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeeSelectedDepartmentsPatch
procedure DefineMessageFields_302;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeSelectedDepartmentsPatch')) do begin
    AddFieldDef(ftoRepeated, ftString, nil, 'SelectedDepartmentIds', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeePositionPatch
procedure DefineMessageFields_303;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeePositionPatch')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Position', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.EmployeeCanBeInvitedForChatPatch
procedure DefineMessageFields_304;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.EmployeeCanBeInvitedForChatPatch')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'CanBeInvitedForChat', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\EmployeeToUpdate.proto
// Diadoc.Api.Proto.Employees.AuthorizationPermissionPatch
procedure DefineMessageFields_305;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.AuthorizationPermissionPatch')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsBlocked', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Subscriptions\Subscription.proto
// Diadoc.Api.Proto.Employees.Subscriptions.EmployeeSubscriptions
procedure DefineMessageFields_306;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.Subscriptions.EmployeeSubscriptions')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.Subscriptions.Subscription'), 'Subscriptions', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Subscriptions\Subscription.proto
// Diadoc.Api.Proto.Employees.Subscriptions.SubscriptionsToUpdate
procedure DefineMessageFields_307;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.Subscriptions.SubscriptionsToUpdate')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Employees.Subscriptions.Subscription'), 'Subscriptions', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Employees\Subscriptions\Subscription.proto
// Diadoc.Api.Proto.Employees.Subscriptions.Subscription
procedure DefineMessageFields_308;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Employees.Subscriptions.Subscription')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Id', 1);
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsSubscribed', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\CancellationInfo.proto
// Diadoc.Api.Proto.Events.CancellationInfo
procedure DefineMessageFields_309;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.CancellationInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Author', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.BoxEventList
procedure DefineMessageFields_310;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.BoxEventList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.BoxEvent'), 'Events', 1);
    AddFieldDef(ftoOptional, ftInt32, nil, 'TotalCount', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.TotalCountType'), 'TotalCountType', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.BoxEvent
procedure DefineMessageFields_311;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.BoxEvent')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'EventId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.Message'), 'Message', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.MessagePatch'), 'Patch', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.Message
procedure DefineMessageFields_312;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.Message')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 1);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'TimestampTicks', 2);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'LastPatchTimestampTicks', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'FromBoxId', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'FromTitle', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'ToBoxId', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'ToTitle', 7);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.Entity'), 'Entities', 8);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDraft', 9);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DraftIsLocked', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DraftIsRecycled', 11);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedFromDraftId', 12);
    AddFieldDef(ftoRepeated, ftString, nil, 'DraftIsTransformedToMessageIdList', 13);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDeleted', 14);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsTest', 15);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsInternal', 16);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsProxified', 17);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyBoxId', 18);
    AddFieldDef(ftoOptional, ftString, nil, 'ProxyTitle', 19);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'PacketIsLocked', 20);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.LockMode'), 'LockMode', 21);
    FieldDef[FieldDefsCount-1].DefaultValue:= 1;
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.MessageType'), 'MessageType', 22);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.TemplateToLetterTransformationInfo'), 'TemplateToLetterTransformationInfo', 23);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReusable', 24);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.Template
procedure DefineMessageFields_313;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.Template')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 1);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'TimestampTicks', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'FromBoxId', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'ToBoxId', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageFromBoxId', 5);
    AddFieldDef(ftoRequired, ftString, nil, 'MessageToBoxId', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.Entity'), 'Entities', 7);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsDeleted', 8);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'MessageToDepartmentId', 9);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.LockMode'), 'LockMode', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageProxyBoxId', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageProxyDepartmentId', 12);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsReusable', 13);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.MessagePatch
procedure DefineMessageFields_314;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.MessagePatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'MessageId', 1);
    AddFieldDef(ftoRequired, ftSfixed64, nil, 'TimestampTicks', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.Entity'), 'Entities', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'ForDraft', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DraftIsRecycled', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftString, nil, 'DraftIsTransformedToMessageIdList', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DraftIsLocked', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'MessageIsDeleted', 8);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.EntityPatch'), 'EntityPatches', 9);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'MessageIsRestored', 10);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'MessageIsDelivered', 11);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'DeliveredPatchId', 12);
    AddFieldDef(ftoRequired, ftString, nil, 'PatchId', 13);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Documents.MessageType'), 'MessageType', 15);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.Entity
procedure DefineMessageFields_315;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.Entity')) do begin
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Events.EntityType'), 'EntityType', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'EntityId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ParentEntityId', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Content'), 'Content', 4);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Events.AttachmentType'), 'AttachmentType', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoOptional, ftString, nil, 'FileName', 6);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedRecipientSignature', 7);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'SignerBoxId', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'NotDeliveredEventId', 10);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Documents.Document'), 'DocumentInfo', 11);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'RawCreationDate', 12);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionInfo'), 'ResolutionInfo', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'SignerDepartmentId', 14);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestInfo'), 'ResolutionRequestInfo', 15);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestDenialInfo'), 'ResolutionRequestDenialInfo', 16);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'NeedReceipt', 17);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'PacketId', 18);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsApprovementSignature', 19);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'IsEncryptedContent', 20);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'AttachmentVersion', 21);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteAssignmentInfo'), 'ResolutionRouteAssignmentInfo', 22);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteRemovalInfo'), 'ResolutionRouteRemovalInfo', 23);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.CancellationInfo'), 'CancellationInfo', 24);
    AddFieldDef(ftoRepeated, ftString, nil, 'Labels', 25);
    AddFieldDef(ftoOptional, ftString, nil, 'Version', 26);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.TemplateTransformationInfo'), 'TemplateTransformationInfo', 27);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.TemplateRefusalInfo'), 'TemplateRefusalInfo', 28);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.OuterDocflowInfo'), 'OuterDocflow', 29);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Events.RevocationRequestInfo'), 'RevocationRequestInfo', 30);
    AddFieldDef(ftoOptional, ftString, nil, 'ContentTypeId', 31);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.EntityPatch
procedure DefineMessageFields_316;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.EntityPatch')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'EntityId', 1);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DocumentIsDeleted', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'MovedToDepartment', 3);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'DocumentIsRestored', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'ContentIsPatched', 5);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftString, nil, 'ForwardedToBoxId', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.TemplateToLetterTransformationInfo
procedure DefineMessageFields_317;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateToLetterTransformationInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'LetterFromBoxId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'LetterToBoxId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'LetterFromDepartmentId', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'LetterToDepartmentId', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'LetterProxyBoxId', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'LetterProxyDepartmentId', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.TemplateTransformationInfo
procedure DefineMessageFields_318;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateTransformationInfo')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'TransformedToDocumentId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Author', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.TemplateRefusalInfo
procedure DefineMessageFields_319;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.TemplateRefusalInfo')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Events.TemplateRefusalType'), 'Type', 1);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoRequired, ftString, nil, 'BoxId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Author', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRequestDenialInfo.proto
// Diadoc.Api.Proto.Events.ResolutionRequestDenialInfo
procedure DefineMessageFields_320;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRequestDenialInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Author', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'InitialRequestId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRouteInfo.proto
// Diadoc.Api.Proto.Events.ResolutionRouteAssignmentInfo
procedure DefineMessageFields_321;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteAssignmentInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'RouteId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Author', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\ResolutionRouteInfo.proto
// Diadoc.Api.Proto.Events.ResolutionRouteRemovalInfo
procedure DefineMessageFields_322;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.ResolutionRouteRemovalInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'RouteId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Author', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\RevocationRequestInfo.proto
// Diadoc.Api.Proto.Events.RevocationRequestInfo
procedure DefineMessageFields_323;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Events.RevocationRequestInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'InitiatorBoxId', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ExternalServiceAuthInfo.proto
// Diadoc.Api.Proto.ExternalServiceAuthInfo
procedure DefineMessageFields_324;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.ExternalServiceAuthInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ServiceUserId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Thumbprint', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardedDocument.proto
// Diadoc.Api.Proto.Forwarding.ForwardedDocumentId
procedure DefineMessageFields_325;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentId')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'FromBoxId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'ForwardEventId', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardedDocument.proto
// Diadoc.Api.Proto.Forwarding.ForwardedDocument
procedure DefineMessageFields_326;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocument')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ForwardTimestamp', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentId'), 'ForwardedDocumentId', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Docflow.DocumentWithDocflow'), 'DocumentWithDocflow', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.ForwardDocumentRequest
procedure DefineMessageFields_327;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardDocumentRequest')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ToBoxId', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'DocumentId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.ForwardDocumentResponse
procedure DefineMessageFields_328;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardDocumentResponse')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'ForwardTimestamp', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentId'), 'ForwardedDocumentId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsRequest
procedure DefineMessageFields_329;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsRequest')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentId'), 'ForwardedDocumentIds', 1);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'InjectEntityContent', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsResponse
procedure DefineMessageFields_330;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocument'), 'ForwardedDocuments', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsRequest
procedure DefineMessageFields_331;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsRequest')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.TimeBasedFilter'), 'Filter', 1);
    AddFieldDef(ftoOptional, ftBytes, nil, 'AfterIndexKey', 2);
    AddFieldDef(ftoOptional, ftBoolean, nil, 'PopulateForwardedDocuments', 3);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
    AddFieldDef(ftoOptional, ftBoolean, nil, 'InjectEntityContent', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= False;
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsResponse
procedure DefineMessageFields_332;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsResponse')) do begin
    AddFieldDef(ftoOptional, ftInt32, nil, 'TotalCount', 1);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentEvent'), 'Events', 2);
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.TotalCountType'), 'TotalCountType', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Forwarding\ForwardingApi.proto
// Diadoc.Api.Proto.Forwarding.ForwardedDocumentEvent
procedure DefineMessageFields_333;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentEvent')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'Timestamp', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocumentId'), 'ForwardedDocumentId', 2);
    AddFieldDef(ftoOptional, ftBytes, nil, 'IndexKey', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Forwarding.ForwardedDocument'), 'ForwardedDocument', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\GetOrganizationsByInnList.proto
// Diadoc.Api.Proto.GetOrganizationsByInnListRequest
procedure DefineMessageFields_334;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.GetOrganizationsByInnListRequest')) do begin
    AddFieldDef(ftoRepeated, ftString, nil, 'InnList', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\GetOrganizationsByInnList.proto
// Diadoc.Api.Proto.OrganizationWithCounteragentStatus
procedure DefineMessageFields_335;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OrganizationWithCounteragentStatus')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Organization'), 'Organization', 1);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.CounteragentStatus'), 'CounteragentStatus', 2);
    FieldDef[FieldDefsCount-1].DefaultValue:= 0;
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'LastEventTimestampTicks', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageFromCounteragent', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'MessageToCounteragent', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.DocumentId'), 'InvitationDocumentId', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\GetOrganizationsByInnList.proto
// Diadoc.Api.Proto.GetOrganizationsByInnListResponse
procedure DefineMessageFields_336;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.GetOrganizationsByInnListResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.OrganizationWithCounteragentStatus'), 'Organizations', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificate552Info.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552SellerTitleInfo
procedure DefineMessageFields_337;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552SellerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Seller', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Buyer', 2);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.GroundInfo'), 'Grounds', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'Currency', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'CurrencyRate', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkDescription'), 'Works', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionDate', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionNumber', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationType', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationTitle', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'GovernmentContractInfo', 16);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 17);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentName', 18);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552TransferInfo'), 'TransferInfo', 19);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificate552Info.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552TransferInfo
procedure DefineMessageFields_338;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552TransferInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'OperationInfo', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferDate', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedThingTransferDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedThingInfo', 4);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificate552Info.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkDescription
procedure DefineMessageFields_339;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkDescription')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'StartingDate', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'CompletionDate', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalVat', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkItem'), 'Items', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificate552Info.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkItem
procedure DefineMessageFields_340;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkItem')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Name', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Description', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitCode', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitName', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Quantity', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Subtotal', 9);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 10);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.TaxRate'), 'TaxRate', 11);
    FieldDef[FieldDefsCount-1].DefaultValue:= 3;
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountDebit', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountCredit', 13);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificate552Info.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552BuyerTitleInfo
procedure DefineMessageFields_341;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552BuyerTitleInfo')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationType', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'OperationContent', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'AcceptanceDate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedThingAcceptDate', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CreatedThingInfo', 9);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 10);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
// Diadoc.Api.Proto.Invoicing.TovTorgSellerTitleInfo
procedure DefineMessageFields_342;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgSellerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Seller', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Buyer', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Shipper', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Consignee', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo'), 'Carrier', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.GroundInfo'), 'Grounds', 7);
    AddFieldDef(ftoRequired, ftString, nil, 'Currency', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'CurrencyRate', 9);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionDate', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'RevisionNumber', 13);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgTransferInfo'), 'TransferInfo', 14);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 16);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationType', 17);
    AddFieldDef(ftoOptional, ftString, nil, 'GovernmentContractInfo', 18);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgTable'), 'Table', 19);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 20);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentName', 21);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
// Diadoc.Api.Proto.Invoicing.TovTorgBuyerTitleInfo
procedure DefineMessageFields_343;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgBuyerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentCreator', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentCreatorBase', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationCode', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'OperationContent', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'AcceptanceDate', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Employee'), 'Employee', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OtherIssuer'), 'OtherIssuer', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfoId'), 'AdditionalInfoId', 8);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner'), 'Signers', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
// Diadoc.Api.Proto.Invoicing.TovTorgTable
procedure DefineMessageFields_344;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgTable')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgItem'), 'Items', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalQuantity', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalGross', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalNet', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalVat', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'Total', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
// Diadoc.Api.Proto.Invoicing.TovTorgItem
procedure DefineMessageFields_345;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgItem')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Product', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Feature', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Sort', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'VendorCode', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'ProductCode', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitName', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'Unit', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'PackageType', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'QuantityInPack', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'Quantity', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'Gross', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'Net', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemToRelease', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 15);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Invoicing.TaxRate'), 'TaxRate', 16);
    FieldDef[FieldDefsCount-1].DefaultValue:= 3;
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 17);
    AddFieldDef(ftoRequired, ftString, nil, 'Subtotal', 18);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountDebit', 19);
    AddFieldDef(ftoOptional, ftString, nil, 'ItemAccountCredit', 20);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 21);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
// Diadoc.Api.Proto.Invoicing.TovTorgTransferInfo
procedure DefineMessageFields_346;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.TovTorgTransferInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'OperationInfo', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'TransferDate', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Attachment', 3);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Waybill'), 'Waybills', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Employee'), 'Employee', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OtherIssuer'), 'OtherIssuer', 6);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AdditionalInfo'), 'AdditionalInfos', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\TovTorgInfo.proto
// Diadoc.Api.Proto.Invoicing.GroundInfo
procedure DefineMessageFields_347;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.GroundInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Number', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Date', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Info', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificateInfo.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSellerTitleInfo
procedure DefineMessageFields_348;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSellerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo'), 'Seller', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DocflowParticipant'), 'Buyer', 2);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentTitle', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.WorkDescription'), 'Works', 6);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo'), 'Signature', 7);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 9);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificateInfo.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificateBuyerTitleInfo
procedure DefineMessageFields_349;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificateBuyerTitleInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Complaints', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo'), 'Signature', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificateInfo.proto
// Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo
procedure DefineMessageFields_350;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'SignatureDate', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'Official', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Attorney'), 'Attorney', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificateInfo.proto
// Diadoc.Api.Proto.Invoicing.WorkDescription
procedure DefineMessageFields_351;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.WorkDescription')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'StartingDate', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'CompletionDate', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 5);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.WorkItem'), 'Items', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\AcceptanceCertificateInfo.proto
// Diadoc.Api.Proto.Invoicing.WorkItem
procedure DefineMessageFields_352;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.WorkItem')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Name', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Description', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitCode', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitName', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Quantity', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'Subtotal', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 10);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Official.proto
// Diadoc.Api.Proto.Invoicing.Official
procedure DefineMessageFields_353;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Official')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Surname', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'FirstName', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Patronymic', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'JobTitle', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Official.proto
// Diadoc.Api.Proto.Invoicing.Attorney
procedure DefineMessageFields_354;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Attorney')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Date', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Number', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'IssuerOrganizationName', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'IssuerPerson', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'IssuerAdditionalInfo', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'RecipientPerson', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'RecipientAdditionalInfo', 7);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\FnsRegistrationMessageInfo.proto
// Diadoc.Api.Proto.Invoicing.FnsRegistrationMessageInfo
procedure DefineMessageFields_355;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.FnsRegistrationMessageInfo')) do begin
    AddFieldDef(ftoRepeated, ftBytes, nil, 'Certificates', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceCorrectionRequestInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceCorrectionRequestInfo
procedure DefineMessageFields_356;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceCorrectionRequestInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ErrorMessage', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\RevocationRequestInfo.proto
// Diadoc.Api.Proto.Invoicing.RevocationRequestInfo
procedure DefineMessageFields_357;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.RevocationRequestInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Comment', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\SignatureRejectionInfo.proto
// Diadoc.Api.Proto.Invoicing.SignatureRejectionInfo
procedure DefineMessageFields_358;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.SignatureRejectionInfo')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'ErrorMessage', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Torg12Info.proto
// Diadoc.Api.Proto.Invoicing.Torg12SellerTitleInfo
procedure DefineMessageFields_359;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Torg12SellerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'DocumentDate', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DocflowParticipant'), 'SellerDocflowParticipant', 3);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.DocflowParticipant'), 'BuyerDocflowParticipant', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo'), 'Shipper', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo'), 'Consignee', 6);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo'), 'Supplier', 7);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.OrganizationInfo'), 'Payer', 8);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Grounds'), 'Grounds', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'WaybillDate', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'WaybillNumber', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'OperationCode', 12);
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Torg12Item'), 'Items', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'ParcelsQuantityTotal', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'ParcelsQuantityTotalInWords', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'GrossQuantityTotal', 16);
    AddFieldDef(ftoOptional, ftString, nil, 'GrossQuantityTotalInWords', 17);
    AddFieldDef(ftoOptional, ftString, nil, 'NetQuantityTotal', 18);
    AddFieldDef(ftoOptional, ftString, nil, 'NetQuantityTotalInWords', 19);
    AddFieldDef(ftoOptional, ftString, nil, 'QuantityTotal', 20);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalWithVatExcluded', 21);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 22);
    AddFieldDef(ftoRequired, ftString, nil, 'Total', 23);
    AddFieldDef(ftoOptional, ftString, nil, 'TotalInWords', 24);
    AddFieldDef(ftoOptional, ftString, nil, 'SupplyDate', 25);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'SupplyAllowedBy', 26);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'SupplyPerformedBy', 27);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'ChiefAccountant', 28);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 29);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 30);
    AddFieldDef(ftoOptional, ftString, nil, 'AttachmentSheetsQuantity', 31);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Torg12Info.proto
// Diadoc.Api.Proto.Invoicing.Torg12BuyerTitleInfo
procedure DefineMessageFields_360;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Torg12BuyerTitleInfo')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'ShipmentReceiptDate', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Attorney'), 'Attorney', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'AcceptedBy', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Official'), 'ReceivedBy', 4);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Invoicing.Signer'), 'Signer', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Torg12Info.proto
// Diadoc.Api.Proto.Invoicing.Torg12Item
procedure DefineMessageFields_361;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Torg12Item')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Feature', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'Sort', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'NomenclatureArticle', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'Code', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'UnitCode', 6);
    AddFieldDef(ftoRequired, ftString, nil, 'UnitName', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'ParcelType', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'ParcelCapacity', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'ParcelsQuantity', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'GrossQuantity', 11);
    AddFieldDef(ftoRequired, ftString, nil, 'Quantity', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'Price', 13);
    AddFieldDef(ftoRequired, ftString, nil, 'TaxRate', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'SubtotalWithVatExcluded', 15);
    AddFieldDef(ftoOptional, ftString, nil, 'Vat', 16);
    AddFieldDef(ftoRequired, ftString, nil, 'Subtotal', 17);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 18);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\Torg12Info.proto
// Diadoc.Api.Proto.Invoicing.Grounds
procedure DefineMessageFields_362;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Invoicing.Grounds')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentName', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentNumber', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'DocumentDate', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'AdditionalInfo', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\KeyValueStorage\KeyValueStorage.proto
// Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry
procedure DefineMessageFields_363;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Key', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Value', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\KeyValueStorage\KeyValueStorage.proto
// Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetRequest
procedure DefineMessageFields_364;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetRequest')) do begin
    AddFieldDef(ftoRepeated, ftString, nil, 'Keys', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\KeyValueStorage\KeyValueStorage.proto
// Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetResponse
procedure DefineMessageFields_365;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetResponse')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry'), 'Entries', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\KeyValueStorage\KeyValueStorage.proto
// Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiPutRequest
procedure DefineMessageFields_366;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiPutRequest')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry'), 'Entries', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\LoginPassword.proto
// Diadoc.Api.Proto.LoginPassword
procedure DefineMessageFields_367;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.LoginPassword')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Login', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Password', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationPropertiesToUpdate.proto
// Diadoc.Api.Proto.StringValue
procedure DefineMessageFields_368;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.StringValue')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Value', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationPropertiesToUpdate.proto
// Diadoc.Api.Proto.OrganizationPropertiesToUpdate
procedure DefineMessageFields_369;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OrganizationPropertiesToUpdate')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'OrgId', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.StringValue'), 'Ogrn', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.StringValue'), 'IfnsCode', 3);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 4);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.HeadOrganizationPropertiesToUpdate'), 'HeadOrganizationProperties', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationPropertiesToUpdate.proto
// Diadoc.Api.Proto.HeadOrganizationPropertiesToUpdate
procedure DefineMessageFields_370;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.HeadOrganizationPropertiesToUpdate')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.StringValue'), 'Kpp', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.StringValue'), 'FullName', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Address'), 'Address', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\AutoBlockStatus.proto
// Diadoc.Api.Proto.Organizations.AutoBlockStatus
procedure DefineMessageFields_371;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Organizations.AutoBlockStatus')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsBlocked', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\BlockStatus.proto
// Diadoc.Api.Proto.Organizations.BlockStatus
procedure DefineMessageFields_372;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Organizations.BlockStatus')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Organizations.ManualBlockStatus'), 'ManualBlockStatus', 1);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Organizations.AutoBlockStatus'), 'AutoBlockStatus', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\ManualBlockStatus.proto
// Diadoc.Api.Proto.Organizations.ManualBlockStatus
procedure DefineMessageFields_373;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Organizations.ManualBlockStatus')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'IsBlocked', 1);
    AddFieldDef(ftoOptional, ftSfixed64, nil, 'RequestedTicks', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organizations\OrganizationFeatures.proto
// Diadoc.Api.Proto.Organizations.OrganizationFeatures
procedure DefineMessageFields_374;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Organizations.OrganizationFeatures')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Organizations.BlockStatus'), 'BlockStatus', 1);
    AddFieldDef(ftoRepeated, ftString, nil, 'Features', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUser.proto
// Diadoc.Api.Proto.OrganizationUser
procedure DefineMessageFields_375;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OrganizationUser')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'Id', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 2);
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.OrganizationUserPermissions'), 'Permissions', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'Position', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUser.proto
// Diadoc.Api.Proto.OrganizationUsersList
procedure DefineMessageFields_376;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.OrganizationUsersList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.OrganizationUser'), 'Users', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'CurrentUserId', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Recognition\Recognition.proto
// Diadoc.Api.Proto.Recognition.Recognized
procedure DefineMessageFields_377;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Recognition.Recognized')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'RecognitionId', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ErrorMessage', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'FileName', 3);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('Diadoc.Api.Proto.Recognition.RecognizedDocumentType'), 'DocumentType', 4);
    FieldDef[FieldDefsCount-1].DefaultValue:= -1;
    AddFieldDef(ftoOptional, ftBytes, nil, 'Content', 5);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Recognition.RecognizedInvoice'), 'Invoice', 6);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Recognition\Recognition.proto
// Diadoc.Api.Proto.Recognition.RecognizedInvoice
procedure DefineMessageFields_378;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Recognition.RecognizedInvoice')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'MetadataJson', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'ValidationErrorMessage', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Registration\RegistrationRequest.proto
// Diadoc.Api.Proto.Registration.RegistrationRequest
procedure DefineMessageFields_379;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Registration.RegistrationRequest')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'CertificateContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Thumbprint', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Registration\RegistrationRequest.proto
// Diadoc.Api.Proto.Registration.RegistrationResponse
procedure DefineMessageFields_380;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Registration.RegistrationResponse')) do begin
    AddFieldDef(ftoRequired, ftEnum, GetProtoType('Diadoc.Api.Proto.Registration.RegistrationStatus'), 'RegistrationStatus', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'BoxId', 2);
    AddFieldDef(ftoOptional, ftBytes, nil, 'DataToSign', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Registration\RegistrationRequest.proto
// Diadoc.Api.Proto.Registration.RegistrationConfirmRequest
procedure DefineMessageFields_381;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Registration.RegistrationConfirmRequest')) do begin
    AddFieldDef(ftoOptional, ftBytes, nil, 'CertificateContent', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Thumbprint', 2);
    AddFieldDef(ftoRequired, ftBytes, nil, 'DataToSign', 3);
    AddFieldDef(ftoRequired, ftBytes, nil, 'Signature', 4);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionRouteList.proto
// Diadoc.Api.Proto.ResolutionRouteList
procedure DefineMessageFields_382;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.ResolutionRouteList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Diadoc.Api.Proto.ResolutionRoute'), 'ResolutionRoutes', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionRouteList.proto
// Diadoc.Api.Proto.ResolutionRoute
procedure DefineMessageFields_383;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.ResolutionRoute')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'RouteId', 1);
    AddFieldDef(ftoRequired, ftString, nil, 'Name', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\RoamingNotification.proto
// Diadoc.Api.Proto.RoamingNotification
procedure DefineMessageFields_384;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.RoamingNotification')) do begin
    AddFieldDef(ftoRequired, ftBoolean, nil, 'Success', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'Description', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\SignatureInfo.proto
// Diadoc.Api.Proto.SignatureInfo
procedure DefineMessageFields_385;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.SignatureInfo')) do begin
    AddFieldDef(ftoRequired, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SigningTime', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Timestamp'), 'SignatureVerificationTime', 2);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.SignatureVerificationResult'), 'SignatureVerificationResult', 3);
    AddFieldDef(ftoRequired, ftString, nil, 'Thumbprint', 4);
    AddFieldDef(ftoRequired, ftString, nil, 'SerialNumber', 5);
    AddFieldDef(ftoOptional, ftString, nil, 'Issuer', 6);
    AddFieldDef(ftoOptional, ftString, nil, 'StartDate', 7);
    AddFieldDef(ftoOptional, ftString, nil, 'EndDate', 8);
    AddFieldDef(ftoOptional, ftString, nil, 'OrgName', 9);
    AddFieldDef(ftoOptional, ftString, nil, 'OrgInn', 10);
    AddFieldDef(ftoOptional, ftString, nil, 'JobTitle', 11);
    AddFieldDef(ftoOptional, ftString, nil, 'FirstName', 12);
    AddFieldDef(ftoOptional, ftString, nil, 'Surname', 13);
    AddFieldDef(ftoOptional, ftString, nil, 'Snils', 14);
    AddFieldDef(ftoOptional, ftString, nil, 'Email', 15);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Users\UserToUpdate.proto
// Diadoc.Api.Proto.Users.UserToUpdate
procedure DefineMessageFields_386;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Users.UserToUpdate')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Users.UserLoginPatch'), 'Login', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.Users.UserFullNamePatch'), 'FullName', 2);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Users\UserToUpdate.proto
// Diadoc.Api.Proto.Users.UserLoginPatch
procedure DefineMessageFields_387;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Users.UserLoginPatch')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'Login', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Users\UserToUpdate.proto
// Diadoc.Api.Proto.Users.UserFullNamePatch
procedure DefineMessageFields_388;
begin
  with TsanPBMessageType(GetProtoType('Diadoc.Api.Proto.Users.UserFullNamePatch')) do begin
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Diadoc.Api.Proto.FullName'), 'FullName', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-PostApi.proto
// Diadoc.Api.Proto.Events.CustomDataPatchOperation
procedure DefineEnumItems_1;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Events.CustomDataPatchOperation')) do begin
    AddEnumItem(0, 'Set');
    AddEnumItem(1, 'Remove');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\LockMode.proto
// Diadoc.Api.Proto.LockMode
procedure DefineEnumItems_2;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.LockMode')) do begin
    AddEnumItem(0, 'Unknown');
    AddEnumItem(1, 'None');
    AddEnumItem(2, 'Send');
    AddEnumItem(3, 'Full');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionRequestType.proto
// Diadoc.Api.Proto.ResolutionRequestType
procedure DefineEnumItems_3;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.ResolutionRequestType')) do begin
    AddEnumItem(-1, 'UnknownResolutionRequestType');
    AddEnumItem(0, 'ApprovementRequest');
    AddEnumItem(1, 'SignatureRequest');
    AddEnumItem(2, 'ApprovementSignatureRequest');
    AddEnumItem(3, 'Custom');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionType.proto
// Diadoc.Api.Proto.ResolutionType
procedure DefineEnumItems_4;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.ResolutionType')) do begin
    AddEnumItem(-1, 'UnknownResolutionType');
    AddEnumItem(0, 'UndefinedResolutionType');
    AddEnumItem(1, 'Approve');
    AddEnumItem(2, 'Disapprove');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.SignerType
procedure DefineEnumItems_5;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerType')) do begin
    AddEnumItem(1, 'LegalEntity');
    AddEnumItem(2, 'IndividualEntity');
    AddEnumItem(3, 'PhysicalPerson');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.SignerPowers
procedure DefineEnumItems_6;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerPowers')) do begin
    AddEnumItem(0, 'InvoiceSigner');
    AddEnumItem(1, 'PersonMadeOperation');
    AddEnumItem(2, 'MadeAndSignOperation');
    AddEnumItem(3, 'PersonDocumentedOperation');
    AddEnumItem(4, 'MadeOperationAndSignedInvoice');
    AddEnumItem(5, 'MadeAndResponsibleForOperationAndSignedInvoice');
    AddEnumItem(6, 'ResponsibleForOperationAndSignerForInvoice');
    AddEnumItem(7, 'ChairmanCommission');
    AddEnumItem(8, 'MemberCommission');
    AddEnumItem(21, 'PersonApprovedDocument');
    AddEnumItem(22, 'PersonConfirmedDocument');
    AddEnumItem(23, 'PersonAgreedOnDocument');
    AddEnumItem(29, 'PersonOtherPower');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.SignerStatus
procedure DefineEnumItems_7;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.SignerStatus')) do begin
    AddEnumItem(1, 'SellerEmployee');
    AddEnumItem(2, 'InformationCreatorEmployee');
    AddEnumItem(3, 'OtherOrganizationEmployee');
    AddEnumItem(4, 'AuthorizedPerson');
    AddEnumItem(5, 'BuyerEmployee');
    AddEnumItem(6, 'InformationCreatorBuyerEmployee');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedSigner.proto
// Diadoc.Api.Proto.Invoicing.Signers.DocumentTitleType
procedure DefineEnumItems_8;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.Signers.DocumentTitleType')) do begin
    AddEnumItem(-1, 'Absent');
    AddEnumItem(0, 'UtdSeller');
    AddEnumItem(1, 'UtdBuyer');
    AddEnumItem(2, 'UcdSeller');
    AddEnumItem(3, 'UcdBuyer');
    AddEnumItem(4, 'TovTorg551Seller');
    AddEnumItem(5, 'TovTorg551Buyer');
    AddEnumItem(6, 'AccCert552Seller');
    AddEnumItem(7, 'AccCert552Buyer');
    AddEnumItem(8, 'Utd820Buyer');
    AddEnumItem(9, 'Torg2Buyer');
    AddEnumItem(10, 'Torg2AdditionalInfo');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\ResolutionAction.proto
// Diadoc.Api.Proto.ResolutionAction
procedure DefineEnumItems_9;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.ResolutionAction')) do begin
    AddEnumItem(0, 'UnknownAction');
    AddEnumItem(1, 'ApproveAction');
    AddEnumItem(2, 'DisapproveAction');
    AddEnumItem(3, 'SignWithApprovementSignature');
    AddEnumItem(4, 'SignWithPrimarySignature');
    AddEnumItem(5, 'DenySignatureRequest');
    AddEnumItem(6, 'RejectSigning');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentType.proto
// Diadoc.Api.Proto.DocumentType
procedure DefineEnumItems_10;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.DocumentType')) do begin
    AddEnumItem(-1, 'UnknownDocumentType');
    AddEnumItem(0, 'Nonformalized');
    AddEnumItem(1, 'Invoice');
    AddEnumItem(11, 'TrustConnectionRequest');
    AddEnumItem(12, 'Torg12');
    AddEnumItem(13, 'InvoiceRevision');
    AddEnumItem(14, 'InvoiceCorrection');
    AddEnumItem(15, 'InvoiceCorrectionRevision');
    AddEnumItem(16, 'AcceptanceCertificate');
    AddEnumItem(18, 'ProformaInvoice');
    AddEnumItem(19, 'XmlTorg12');
    AddEnumItem(20, 'XmlAcceptanceCertificate');
    AddEnumItem(26, 'PriceList');
    AddEnumItem(30, 'PriceListAgreement');
    AddEnumItem(34, 'CertificateRegistry');
    AddEnumItem(35, 'ReconciliationAct');
    AddEnumItem(36, 'Contract');
    AddEnumItem(37, 'Torg13');
    AddEnumItem(38, 'ServiceDetails');
    AddEnumItem(40, 'SupplementaryAgreement');
    AddEnumItem(41, 'UniversalTransferDocument');
    AddEnumItem(45, 'UniversalTransferDocumentRevision');
    AddEnumItem(49, 'UniversalCorrectionDocument');
    AddEnumItem(50, 'UniversalCorrectionDocumentRevision');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\DocumentDirection.proto
// Diadoc.Api.Proto.DocumentDirection
procedure DefineEnumItems_11;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.DocumentDirection')) do begin
    AddEnumItem(0, 'UnknownDocumentDirection');
    AddEnumItem(1, 'Inbound');
    AddEnumItem(2, 'Outbound');
    AddEnumItem(3, 'Internal');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.FunctionType
procedure DefineEnumItems_12;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.FunctionType')) do begin
    AddEnumItem(0, 'Invoice');
    AddEnumItem(1, 'Basic');
    AddEnumItem(2, 'InvoiceAndBasic');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\UniversalTransferDocumentInfo.proto
// Diadoc.Api.Proto.Invoicing.ItemMark
procedure DefineEnumItems_13;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.ItemMark')) do begin
    AddEnumItem(0, 'NotSpecified');
    AddEnumItem(1, 'Property');
    AddEnumItem(2, 'Job');
    AddEnumItem(3, 'Service');
    AddEnumItem(4, 'PropertyRights');
    AddEnumItem(5, 'Other');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.InvoiceFormatVersion
procedure DefineEnumItems_14;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.InvoiceFormatVersion')) do begin
    AddEnumItem(0, 'DefaultInvoiceFormatVersion');
    AddEnumItem(1, 'v5_01');
    AddEnumItem(2, 'v5_02');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\InvoiceInfo.proto
// Diadoc.Api.Proto.Invoicing.TaxRate
procedure DefineEnumItems_15;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.TaxRate')) do begin
    AddEnumItem(0, 'NoVat');
    AddEnumItem(1, 'Percent_0');
    AddEnumItem(2, 'Percent_10');
    AddEnumItem(3, 'Percent_18');
    AddEnumItem(4, 'Percent_20');
    AddEnumItem(5, 'Fraction_10_110');
    AddEnumItem(6, 'Fraction_18_118');
    AddEnumItem(7, 'TaxedByAgent');
    AddEnumItem(8, 'Fraction_20_120');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Invoicing\ExtendedOrganizationInfo.proto
// Diadoc.Api.Proto.Invoicing.Organizations.OrgType
procedure DefineEnumItems_16;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Invoicing.Organizations.OrgType')) do begin
    AddEnumItem(1, 'LegalEntity');
    AddEnumItem(2, 'IndividualEntity');
    AddEnumItem(3, 'ForeignEntity');
    AddEnumItem(4, 'PhysicalEntity');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Certificates\CertificateInfoV2.proto
// Diadoc.Api.Proto.Certificates.CertificateType
procedure DefineEnumItems_17;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Certificates.CertificateType')) do begin
    AddEnumItem(0, 'Unknown');
    AddEnumItem(1, 'Token');
    AddEnumItem(2, 'Dss');
    AddEnumItem(3, 'KonturCertificate');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Counteragent.proto
// Diadoc.Api.Proto.CounteragentStatus
procedure DefineEnumItems_18;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.CounteragentStatus')) do begin
    AddEnumItem(0, 'UnknownCounteragentStatus');
    AddEnumItem(1, 'IsMyCounteragent');
    AddEnumItem(2, 'InvitesMe');
    AddEnumItem(3, 'IsInvitedByMe');
    AddEnumItem(5, 'RejectsMe');
    AddEnumItem(6, 'IsRejectedByMe');
    AddEnumItem(7, 'NotInCounteragentList');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
// Diadoc.Api.Proto.OrganizationInvoiceFormatVersion
procedure DefineEnumItems_19;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.OrganizationInvoiceFormatVersion')) do begin
    AddEnumItem(1, 'v5_01');
    AddEnumItem(2, 'v5_02');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Organization.proto
// Diadoc.Api.Proto.Sociability
procedure DefineEnumItems_20;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Sociability')) do begin
    AddEnumItem(0, 'AllOrganizations');
    AddEnumItem(1, 'CounteragentsOnly');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\Docflow.proto
// Diadoc.Api.Proto.Docflow.DocflowStatusSeverity
procedure DefineEnumItems_21;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Docflow.DocflowStatusSeverity')) do begin
    AddEnumItem(0, 'UnknownDocflowStatusSeverity');
    AddEnumItem(1, 'Info');
    AddEnumItem(2, 'Success');
    AddEnumItem(3, 'Warning');
    AddEnumItem(4, 'Error');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\DocflowApi.proto
// Diadoc.Api.Proto.Docflow.SearchScope
procedure DefineEnumItems_22;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Docflow.SearchScope')) do begin
    AddEnumItem(0, 'SearchScopeAny');
    AddEnumItem(1, 'SearchScopeIncoming');
    AddEnumItem(2, 'SearchScopeOutgoing');
    AddEnumItem(3, 'SearchScopeDeleted');
    AddEnumItem(4, 'SearchScopeInternal');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\TotalCountType.proto
// Diadoc.Api.Proto.TotalCountType
procedure DefineEnumItems_23;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.TotalCountType')) do begin
    AddEnumItem(0, 'UnknownCountType');
    AddEnumItem(1, 'Equal');
    AddEnumItem(2, 'GreaterThanOrEqual');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\TimeBasedFilter.proto
// Diadoc.Api.Proto.SortDirection
procedure DefineEnumItems_24;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.SortDirection')) do begin
    AddEnumItem(0, 'UnknownSortDirection');
    AddEnumItem(1, 'Ascending');
    AddEnumItem(2, 'Descending');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.ResolutionStatusType
procedure DefineEnumItems_25;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.ResolutionStatusType')) do begin
    AddEnumItem(-1, 'UnknownResolutionStatus');
    AddEnumItem(0, 'None');
    AddEnumItem(1, 'Approved');
    AddEnumItem(2, 'Disapproved');
    AddEnumItem(3, 'ApprovementRequested');
    AddEnumItem(4, 'SignatureRequested');
    AddEnumItem(5, 'SignatureDenied');
    AddEnumItem(6, 'ActionsRequested');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.RevocationStatus
procedure DefineEnumItems_26;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.RevocationStatus')) do begin
    AddEnumItem(0, 'UnknownRevocationStatus');
    AddEnumItem(1, 'RevocationStatusNone');
    AddEnumItem(2, 'RevocationIsRequestedByMe');
    AddEnumItem(3, 'RequestsMyRevocation');
    AddEnumItem(4, 'RevocationAccepted');
    AddEnumItem(5, 'RevocationRejected');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.RoamingNotificationStatus
procedure DefineEnumItems_27;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.RoamingNotificationStatus')) do begin
    AddEnumItem(0, 'UnknownRoamingNotificationStatus');
    AddEnumItem(1, 'RoamingNotificationStatusNone');
    AddEnumItem(2, 'RoamingNotificationStatusSuccess');
    AddEnumItem(3, 'RoamingNotificationStatusError');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.SenderSignatureStatus
procedure DefineEnumItems_28;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.SenderSignatureStatus')) do begin
    AddEnumItem(0, 'UnknownSenderSignatureStatus');
    AddEnumItem(1, 'WaitingForSenderSignature');
    AddEnumItem(2, 'SenderSignatureUnchecked');
    AddEnumItem(3, 'SenderSignatureCheckedAndValid');
    AddEnumItem(4, 'SenderSignatureCheckedAndInvalid');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.ProxySignatureStatus
procedure DefineEnumItems_29;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.ProxySignatureStatus')) do begin
    AddEnumItem(0, 'UnknownProxySignatureStatus');
    AddEnumItem(1, 'ProxySignatureStatusNone');
    AddEnumItem(2, 'WaitingForProxySignature');
    AddEnumItem(3, 'WithProxySignature');
    AddEnumItem(4, 'ProxySignatureRejected');
    AddEnumItem(5, 'InvalidProxySignature');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.GeneralReceiptStatus
procedure DefineEnumItems_30;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.GeneralReceiptStatus')) do begin
    AddEnumItem(0, 'GeneralReceiptStatusUnknown');
    AddEnumItem(1, 'GeneralReceiptStatusNotAcceptable');
    AddEnumItem(2, 'HaveToCreateReceipt');
    AddEnumItem(3, 'WaitingForReceipt');
    AddEnumItem(4, 'Finished');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.RecipientResponseStatus
procedure DefineEnumItems_31;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.RecipientResponseStatus')) do begin
    AddEnumItem(0, 'RecipientResponseStatusUnknown');
    AddEnumItem(1, 'RecipientResponseStatusNotAcceptable');
    AddEnumItem(2, 'WaitingForRecipientSignature');
    AddEnumItem(3, 'WithRecipientSignature');
    AddEnumItem(4, 'RecipientSignatureRequestRejected');
    AddEnumItem(5, 'InvalidRecipientSignature');
    AddEnumItem(6, 'WithRecipientPartiallySignature');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\Document.proto
// Diadoc.Api.Proto.Documents.MessageType
procedure DefineEnumItems_32;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.MessageType')) do begin
    AddEnumItem(0, 'Unknown');
    AddEnumItem(1, 'Letter');
    AddEnumItem(2, 'Draft');
    AddEnumItem(3, 'Template');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\AcceptanceCertificateDocument.proto
// Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateDocumentStatus
procedure DefineEnumItems_33;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateDocumentStatus')) do begin
    AddEnumItem(0, 'UnknownAcceptanceCertificateDocumentStatus');
    AddEnumItem(1, 'OutboundWaitingForRecipientSignature');
    AddEnumItem(2, 'OutboundWithRecipientSignature');
    AddEnumItem(19, 'OutboundWithRecipientPartiallySignature');
    AddEnumItem(3, 'OutboundRecipientSignatureRequestRejected');
    AddEnumItem(10, 'OutboundWaitingForSenderSignature');
    AddEnumItem(11, 'OutboundInvalidSenderSignature');
    AddEnumItem(16, 'OutboundNoRecipientSignatureRequest');
    AddEnumItem(4, 'InboundWaitingForRecipientSignature');
    AddEnumItem(5, 'InboundWithRecipientSignature');
    AddEnumItem(20, 'InboundWithRecipientPartiallySignature');
    AddEnumItem(6, 'InboundRecipientSignatureRequestRejected');
    AddEnumItem(12, 'InboundInvalidRecipientSignature');
    AddEnumItem(17, 'InboundNoRecipientSignatureRequest');
    AddEnumItem(7, 'InternalWaitingForRecipientSignature');
    AddEnumItem(8, 'InternalWithRecipientSignature');
    AddEnumItem(21, 'InternalWithRecipientPartiallySignature');
    AddEnumItem(9, 'InternalRecipientSignatureRequestRejected');
    AddEnumItem(13, 'InternalWaitingForSenderSignature');
    AddEnumItem(14, 'InternalInvalidSenderSignature');
    AddEnumItem(15, 'InternalInvalidRecipientSignature');
    AddEnumItem(18, 'InternalNoRecipientSignatureRequest');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\ReceiptStatus.proto
// Diadoc.Api.Proto.Documents.ReceiptStatus
procedure DefineEnumItems_34;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.ReceiptStatus')) do begin
    AddEnumItem(0, 'UnknownReceiptStatus');
    AddEnumItem(1, 'ReceiptStatusNone');
    AddEnumItem(2, 'ReceiptStatusFinished');
    AddEnumItem(3, 'ReceiptStatusHaveToCreateReceipt');
    AddEnumItem(4, 'ReceiptStatusWaitingForReceipt');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\BilateralDocument.proto
// Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus
procedure DefineEnumItems_35;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus')) do begin
    AddEnumItem(0, 'UnknownBilateralDocumentStatus');
    AddEnumItem(1, 'OutboundWaitingForRecipientSignature');
    AddEnumItem(2, 'OutboundWithRecipientSignature');
    AddEnumItem(19, 'OutboundWithRecipientPartiallySignature');
    AddEnumItem(3, 'OutboundRecipientSignatureRequestRejected');
    AddEnumItem(10, 'OutboundWaitingForSenderSignature');
    AddEnumItem(11, 'OutboundInvalidSenderSignature');
    AddEnumItem(4, 'InboundWaitingForRecipientSignature');
    AddEnumItem(5, 'InboundWithRecipientSignature');
    AddEnumItem(20, 'InboundWithRecipientPartiallySignature');
    AddEnumItem(6, 'InboundRecipientSignatureRequestRejected');
    AddEnumItem(12, 'InboundInvalidRecipientSignature');
    AddEnumItem(7, 'InternalWaitingForRecipientSignature');
    AddEnumItem(8, 'InternalWithRecipientSignature');
    AddEnumItem(21, 'InternalWithRecipientPartiallySignature');
    AddEnumItem(9, 'InternalRecipientSignatureRequestRejected');
    AddEnumItem(13, 'InternalWaitingForSenderSignature');
    AddEnumItem(14, 'InternalInvalidSenderSignature');
    AddEnumItem(15, 'InternalInvalidRecipientSignature');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\InvoiceDocument.proto
// Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus
procedure DefineEnumItems_36;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus')) do begin
    AddEnumItem(0, 'UnknownInvoiceStatus');
    AddEnumItem(1, 'OutboundWaitingForInvoiceReceipt');
    AddEnumItem(2, 'OutboundNotFinished');
    AddEnumItem(3, 'OutboundFinished');
    AddEnumItem(6, 'OutboundWaitingForSenderSignature');
    AddEnumItem(7, 'OutboundInvalidSenderSignature');
    AddEnumItem(4, 'InboundNotFinished');
    AddEnumItem(5, 'InboundFinished');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\NonformalizedDocument.proto
// Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentStatus
procedure DefineEnumItems_37;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentStatus')) do begin
    AddEnumItem(0, 'UnknownNonformalizedDocumentStatus');
    AddEnumItem(1, 'OutboundNoRecipientSignatureRequest');
    AddEnumItem(2, 'OutboundWaitingForRecipientSignature');
    AddEnumItem(3, 'OutboundWithRecipientSignature');
    AddEnumItem(4, 'OutboundRecipientSignatureRequestRejected');
    AddEnumItem(13, 'OutboundWaitingForSenderSignature');
    AddEnumItem(14, 'OutboundInvalidSenderSignature');
    AddEnumItem(5, 'InboundNoRecipientSignatureRequest');
    AddEnumItem(6, 'InboundWaitingForRecipientSignature');
    AddEnumItem(7, 'InboundWithRecipientSignature');
    AddEnumItem(8, 'InboundRecipientSignatureRequestRejected');
    AddEnumItem(15, 'InboundInvalidRecipientSignature');
    AddEnumItem(9, 'InternalNoRecipientSignatureRequest');
    AddEnumItem(10, 'InternalWaitingForRecipientSignature');
    AddEnumItem(11, 'InternalWithRecipientSignature');
    AddEnumItem(12, 'InternalRecipientSignatureRequestRejected');
    AddEnumItem(16, 'InternalWaitingForSenderSignature');
    AddEnumItem(17, 'InternalInvalidSenderSignature');
    AddEnumItem(18, 'InternalInvalidRecipientSignature');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UnilateralDocument.proto
// Diadoc.Api.Proto.Documents.UnilateralDocument.UnilateralDocumentStatus
procedure DefineEnumItems_38;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.UnilateralDocument.UnilateralDocumentStatus')) do begin
    AddEnumItem(0, 'UnknownUnilateralDocumentStatus');
    AddEnumItem(1, 'Outbound');
    AddEnumItem(4, 'OutboundWaitingForSenderSignature');
    AddEnumItem(5, 'OutboundInvalidSenderSignature');
    AddEnumItem(2, 'Inbound');
    AddEnumItem(3, 'Internal');
    AddEnumItem(6, 'InternalWaitingForSenderSignature');
    AddEnumItem(7, 'InternalInvalidSenderSignature');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Documents\UniversalTransferDocument.proto
// Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus
procedure DefineEnumItems_39;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus')) do begin
    AddEnumItem(0, 'UnknownDocumentStatus');
    AddEnumItem(1, 'OutboundWaitingForSenderSignature');
    AddEnumItem(2, 'OutboundWaitingForInvoiceReceiptAndRecipientSignature');
    AddEnumItem(3, 'OutboundWaitingForInvoiceReceipt');
    AddEnumItem(4, 'OutboundWaitingForRecipientSignature');
    AddEnumItem(5, 'OutboundWithRecipientSignature');
    AddEnumItem(6, 'OutboundRecipientSignatureRequestRejected');
    AddEnumItem(7, 'OutboundInvalidSenderSignature');
    AddEnumItem(8, 'OutboundNotFinished');
    AddEnumItem(9, 'OutboundFinished');
    AddEnumItem(16, 'InboundWaitingForRecipientSignature');
    AddEnumItem(17, 'InboundWithRecipientSignature');
    AddEnumItem(18, 'InboundRecipientSignatureRequestRejected');
    AddEnumItem(19, 'InboundInvalidRecipientSignature');
    AddEnumItem(20, 'InboundNotFinished');
    AddEnumItem(21, 'InboundFinished');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OuterDocflowStatus.proto
// Diadoc.Api.Proto.OuterStatusType
procedure DefineEnumItems_40;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.OuterStatusType')) do begin
    AddEnumItem(0, 'UnknownStatus');
    AddEnumItem(1, 'Normal');
    AddEnumItem(2, 'Success');
    AddEnumItem(3, 'Warning');
    AddEnumItem(4, 'Error');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Docflow\ResolutionDocflowV3.proto
// Diadoc.Api.Proto.Docflow.ResolutionStatus
procedure DefineEnumItems_41;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Docflow.ResolutionStatus')) do begin
    AddEnumItem(0, 'UnknownStatus');
    AddEnumItem(1, 'None');
    AddEnumItem(2, 'Approved');
    AddEnumItem(3, 'Disapproved');
    AddEnumItem(4, 'ApprovementRequested');
    AddEnumItem(5, 'ApprovementSignatureRequested');
    AddEnumItem(6, 'PrimarySignatureRequested');
    AddEnumItem(7, 'SignatureRequestRejected');
    AddEnumItem(8, 'SignedWithApprovingSignature');
    AddEnumItem(9, 'SignedWithPrimarySignature');
    AddEnumItem(10, 'PrimarySignatureRejected');
    AddEnumItem(11, 'ActionsRequested');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssConfirmType
procedure DefineEnumItems_42;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Dss.DssConfirmType')) do begin
    AddEnumItem(-1, 'ConfirmTypeUnknown');
    AddEnumItem(0, 'None');
    AddEnumItem(1, 'Sms');
    AddEnumItem(2, 'MyDss');
    AddEnumItem(3, 'Applet');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssOperator
procedure DefineEnumItems_43;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Dss.DssOperator')) do begin
    AddEnumItem(0, 'OperatorUnknown');
    AddEnumItem(1, 'Megafon');
    AddEnumItem(2, 'Kontur');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssFileSigningStatus
procedure DefineEnumItems_44;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Dss.DssFileSigningStatus')) do begin
    AddEnumItem(0, 'UnknownSigningStatus');
    AddEnumItem(1, 'SigningCompleted');
    AddEnumItem(2, 'SigningError');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Dss\DssSign.proto
// Diadoc.Api.Proto.Dss.DssOperationStatus
procedure DefineEnumItems_45;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Dss.DssOperationStatus')) do begin
    AddEnumItem(0, 'Unknown');
    AddEnumItem(1, 'InProgress');
    AddEnumItem(2, 'Completed');
    AddEnumItem(3, 'CanceledByUser');
    AddEnumItem(4, 'Timeout');
    AddEnumItem(5, 'Crashed');
    AddEnumItem(6, 'UserHasUnconfirmedOperation');
    AddEnumItem(7, 'OperationRetryRequired');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\OrganizationUserPermissions.proto
// Diadoc.Api.Proto.DocumentAccessLevel
procedure DefineEnumItems_46;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.DocumentAccessLevel')) do begin
    AddEnumItem(-1, 'UnknownDocumentAccessLevel');
    AddEnumItem(0, 'DepartmentOnly');
    AddEnumItem(1, 'DepartmentAndSubdepartments');
    AddEnumItem(2, 'AllDocuments');
    AddEnumItem(3, 'SelectedDepartments');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.TemplateRefusalType
procedure DefineEnumItems_47;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Events.TemplateRefusalType')) do begin
    AddEnumItem(0, 'UnknownTemplateRefusalType');
    AddEnumItem(1, 'Refusal');
    AddEnumItem(2, 'Withdrawal');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.EntityType
procedure DefineEnumItems_48;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Events.EntityType')) do begin
    AddEnumItem(0, 'UnknownEntityType');
    AddEnumItem(1, 'Attachment');
    AddEnumItem(2, 'Signature');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Events\DiadocMessage-GetApi.proto
// Diadoc.Api.Proto.Events.AttachmentType
procedure DefineEnumItems_49;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Events.AttachmentType')) do begin
    AddEnumItem(-1, 'UnknownAttachmentType');
    AddEnumItem(0, 'Nonformalized');
    AddEnumItem(1, 'Invoice');
    AddEnumItem(2, 'InvoiceReceipt');
    AddEnumItem(3, 'InvoiceConfirmation');
    AddEnumItem(4, 'InvoiceCorrectionRequest');
    AddEnumItem(5, 'AttachmentComment');
    AddEnumItem(6, 'DeliveryFailureNotification');
    AddEnumItem(7, 'EancomInvoic');
    AddEnumItem(8, 'SignatureRequestRejection');
    AddEnumItem(9, 'EcrCatConformanceCertificateMetadata');
    AddEnumItem(10, 'SignatureVerificationReport');
    AddEnumItem(11, 'TrustConnectionRequest');
    AddEnumItem(12, 'Torg12');
    AddEnumItem(13, 'InvoiceRevision');
    AddEnumItem(14, 'InvoiceCorrection');
    AddEnumItem(15, 'InvoiceCorrectionRevision');
    AddEnumItem(16, 'AcceptanceCertificate');
    AddEnumItem(17, 'StructuredData');
    AddEnumItem(18, 'ProformaInvoice');
    AddEnumItem(19, 'XmlTorg12');
    AddEnumItem(20, 'XmlAcceptanceCertificate');
    AddEnumItem(21, 'XmlTorg12BuyerTitle');
    AddEnumItem(22, 'XmlAcceptanceCertificateBuyerTitle');
    AddEnumItem(23, 'Resolution');
    AddEnumItem(24, 'ResolutionRequest');
    AddEnumItem(25, 'ResolutionRequestDenial');
    AddEnumItem(26, 'PriceList');
    AddEnumItem(27, 'Receipt');
    AddEnumItem(28, 'XmlSignatureRejection');
    AddEnumItem(29, 'RevocationRequest');
    AddEnumItem(30, 'PriceListAgreement');
    AddEnumItem(34, 'CertificateRegistry');
    AddEnumItem(35, 'ReconciliationAct');
    AddEnumItem(36, 'Contract');
    AddEnumItem(37, 'Torg13');
    AddEnumItem(38, 'ServiceDetails');
    AddEnumItem(39, 'RoamingNotification');
    AddEnumItem(40, 'SupplementaryAgreement');
    AddEnumItem(41, 'UniversalTransferDocument');
    AddEnumItem(42, 'UniversalTransferDocumentBuyerTitle');
    AddEnumItem(45, 'UniversalTransferDocumentRevision');
    AddEnumItem(49, 'UniversalCorrectionDocument');
    AddEnumItem(50, 'UniversalCorrectionDocumentRevision');
    AddEnumItem(51, 'UniversalCorrectionDocumentBuyerTitle');
    AddEnumItem(64, 'CustomData');
    AddEnumItem(65, 'MoveDocument');
    AddEnumItem(66, 'ResolutionRouteAssignmentAttachment');
    AddEnumItem(67, 'ResolutionRouteRemovalAttachment');
    AddEnumItem(68, 'Title');
    AddEnumItem(69, 'Cancellation');
    AddEnumItem(71, 'Edition');
    AddEnumItem(72, 'DeletionRestoration');
    AddEnumItem(73, 'TemplateTransformation');
    AddEnumItem(74, 'TemplateRefusal');
    AddEnumItem(75, 'OuterDocflow');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Recognition\Recognition.proto
// Diadoc.Api.Proto.Recognition.RecognizedDocumentType
procedure DefineEnumItems_50;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Recognition.RecognizedDocumentType')) do begin
    AddEnumItem(-1, 'UnknownRecognizedDocumentType');
    AddEnumItem(1, 'Invoice');
  end;
end;

// C:\MyProjects\EXAMPLES\Example08_KonturDiadoc\Diadoc_proto_files\Registration\RegistrationRequest.proto
// Diadoc.Api.Proto.Registration.RegistrationStatus
procedure DefineEnumItems_51;
begin
  with TsanPBEnumType(GetProtoType('Diadoc.Api.Proto.Registration.RegistrationStatus')) do begin
    AddEnumItem(0, 'Unknown');
    AddEnumItem(1, 'AccessIsDenied');
    AddEnumItem(2, 'AccessRequestIsRejected');
    AddEnumItem(3, 'CertificateOwnershipProofIsRequired');
    AddEnumItem(4, 'CertificateIsNotQualified');
    AddEnumItem(5, 'RegistrationIsCompleted');
    AddEnumItem(6, 'RegistrationIsInProgress');
    AddEnumItem(7, 'RegistrationInBranchIsForbidden');
    AddEnumItem(8, 'AccessRequestIsPending');
    AddEnumItem(9, 'OrganizationNotFound');
  end;
end;

procedure CreateTypes;
var
  I: integer;
begin

  ProtoTypeList:= TList<TsanPBCustomType>.Create;

  for I:= 1 to Length(PROTO_MESSAGE_NAMES) do begin
    ProtoTypeList.Add(TsanPBMessageType.Create(nil, PROTO_MESSAGE_NAMES[I-1]));
  end;

  for I:= 1 to Length(PROTO_ENUM_NAMES) do begin
    ProtoTypeList.Add(TsanPBEnumType.Create(nil, PROTO_ENUM_NAMES[I-1]));
  end;

  DefineMessageFields_1; //Diadoc.Api.Proto.AcquireCounteragentRequest
  DefineMessageFields_2; //Diadoc.Api.Proto.InvitationDocument
  DefineMessageFields_3; //Diadoc.Api.Proto.AcquireCounteragentResult
  DefineMessageFields_4; //Diadoc.Api.Proto.Events.MessageToPost
  DefineMessageFields_5; //Diadoc.Api.Proto.Events.EncryptedXmlDocumentAttachment
  DefineMessageFields_6; //Diadoc.Api.Proto.Events.EncryptedInvoiceAttachment
  DefineMessageFields_7; //Diadoc.Api.Proto.Events.EncryptedDocumentMetadata
  DefineMessageFields_8; //Diadoc.Api.Proto.Events.EncryptedXmlBasicDocumentMetadata
  DefineMessageFields_9; //Diadoc.Api.Proto.Events.EncryptedInvoiceMetadata
  DefineMessageFields_10; //Diadoc.Api.Proto.Events.EncryptedInvoiceCorrectionMetadata
  DefineMessageFields_11; //Diadoc.Api.Proto.Events.XmlDocumentAttachment
  DefineMessageFields_12; //Diadoc.Api.Proto.Events.NonformalizedAttachment
  DefineMessageFields_13; //Diadoc.Api.Proto.Events.BasicDocumentAttachment
  DefineMessageFields_14; //Diadoc.Api.Proto.Events.Torg13Attachment
  DefineMessageFields_15; //Diadoc.Api.Proto.Events.AcceptanceCertificateAttachment
  DefineMessageFields_16; //Diadoc.Api.Proto.Events.TrustConnectionRequestAttachment
  DefineMessageFields_17; //Diadoc.Api.Proto.Events.StructuredDataAttachment
  DefineMessageFields_18; //Diadoc.Api.Proto.Events.PriceListAttachment
  DefineMessageFields_19; //Diadoc.Api.Proto.Events.ReconciliationActAttachment
  DefineMessageFields_20; //Diadoc.Api.Proto.Events.ContractAttachment
  DefineMessageFields_21; //Diadoc.Api.Proto.Events.SupplementaryAgreementAttachment
  DefineMessageFields_22; //Diadoc.Api.Proto.Events.ServiceDetailsAttachment
  DefineMessageFields_23; //Diadoc.Api.Proto.Events.DocumentAttachment
  DefineMessageFields_24; //Diadoc.Api.Proto.Events.MetadataItem
  DefineMessageFields_25; //Diadoc.Api.Proto.Events.MessagePatchToPost
  DefineMessageFields_26; //Diadoc.Api.Proto.Events.EditingPatch
  DefineMessageFields_27; //Diadoc.Api.Proto.Events.SignatureVerification
  DefineMessageFields_28; //Diadoc.Api.Proto.Events.ResolutionRequestAttachment
  DefineMessageFields_29; //Diadoc.Api.Proto.Events.ResolutionRouteAssignment
  DefineMessageFields_30; //Diadoc.Api.Proto.Events.ResolutionRequestCancellationAttachment
  DefineMessageFields_31; //Diadoc.Api.Proto.Events.ResolutionRequestDenialCancellationAttachment
  DefineMessageFields_32; //Diadoc.Api.Proto.Events.ResolutionRequestDenialAttachment
  DefineMessageFields_33; //Diadoc.Api.Proto.Events.ResolutionAttachment
  DefineMessageFields_34; //Diadoc.Api.Proto.Events.ReceiptAttachment
  DefineMessageFields_35; //Diadoc.Api.Proto.Events.RecipientTitleAttachment
  DefineMessageFields_36; //Diadoc.Api.Proto.Events.CorrectionRequestAttachment
  DefineMessageFields_37; //Diadoc.Api.Proto.Events.DocumentSignature
  DefineMessageFields_38; //Diadoc.Api.Proto.Events.DocumentSenderSignature
  DefineMessageFields_39; //Diadoc.Api.Proto.Events.RequestedSignatureRejection
  DefineMessageFields_40; //Diadoc.Api.Proto.Events.SignedContent
  DefineMessageFields_41; //Diadoc.Api.Proto.Events.DraftToSend
  DefineMessageFields_42; //Diadoc.Api.Proto.Events.PrepareDocumentsToSignRequest
  DefineMessageFields_43; //Diadoc.Api.Proto.Events.DraftDocumentToPatch
  DefineMessageFields_44; //Diadoc.Api.Proto.Events.ContentToPatch
  DefineMessageFields_45; //Diadoc.Api.Proto.Events.DocumentToPatch
  DefineMessageFields_46; //Diadoc.Api.Proto.Events.DocumentPatchedContent
  DefineMessageFields_47; //Diadoc.Api.Proto.Events.PrepareDocumentsToSignResponse
  DefineMessageFields_48; //Diadoc.Api.Proto.Events.MessageToSend
  DefineMessageFields_49; //Diadoc.Api.Proto.Events.RevocationRequestAttachment
  DefineMessageFields_50; //Diadoc.Api.Proto.Events.XmlSignatureRejectionAttachment
  DefineMessageFields_51; //Diadoc.Api.Proto.Events.RoamingNotificationToPost
  DefineMessageFields_52; //Diadoc.Api.Proto.Events.CustomDataPatch
  DefineMessageFields_53; //Diadoc.Api.Proto.Events.EditDocumentPacketCommand
  DefineMessageFields_54; //Diadoc.Api.Proto.Events.ResolutionRouteRemoval
  DefineMessageFields_55; //Diadoc.Api.Proto.Events.TemplateToPost
  DefineMessageFields_56; //Diadoc.Api.Proto.Events.TemplateDocumentAttachment
  DefineMessageFields_57; //Diadoc.Api.Proto.Events.TemplatePatchToPost
  DefineMessageFields_58; //Diadoc.Api.Proto.Events.TemplateRefusalAttachment
  DefineMessageFields_59; //Diadoc.Api.Proto.Events.PredefinedRecipientTitle
  DefineMessageFields_60; //Diadoc.Api.Proto.Events.UnsignedContent
  DefineMessageFields_61; //Diadoc.Api.Proto.Events.TemplateTransformationToPost
  DefineMessageFields_62; //Diadoc.Api.Proto.Events.DocumentTransformation
  DefineMessageFields_63; //Diadoc.Api.Proto.DocumentId
  DefineMessageFields_64; //Diadoc.Api.Proto.DocumentIdEx
  DefineMessageFields_65; //Diadoc.Api.Proto.CustomDataItem
  DefineMessageFields_66; //Diadoc.Api.Proto.Invoicing.Signer
  DefineMessageFields_67; //Diadoc.Api.Proto.Invoicing.SignerDetails
  DefineMessageFields_68; //Diadoc.Api.Proto.Invoicing.Signers.ExtendedSigner
  DefineMessageFields_69; //Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetails
  DefineMessageFields_70; //Diadoc.Api.Proto.Invoicing.Signers.ExtendedSignerDetailsToPost
  DefineMessageFields_71; //Diadoc.Api.Proto.Events.ResolutionInfo
  DefineMessageFields_72; //Diadoc.Api.Proto.Events.ResolutionRequestInfo
  DefineMessageFields_73; //Diadoc.Api.Proto.ResolutionTarget
  DefineMessageFields_74; //Diadoc.Api.Proto.Invoicing.DocflowParticipant
  DefineMessageFields_75; //Diadoc.Api.Proto.Invoicing.DiadocOrganizationInfo
  DefineMessageFields_76; //Diadoc.Api.Proto.Invoicing.OrganizationInfo
  DefineMessageFields_77; //Diadoc.Api.Proto.Address
  DefineMessageFields_78; //Diadoc.Api.Proto.RussianAddress
  DefineMessageFields_79; //Diadoc.Api.Proto.ForeignAddress
  DefineMessageFields_80; //Diadoc.Api.Proto.Docflow.DocumentInfo
  DefineMessageFields_81; //Diadoc.Api.Proto.Docflow.DocumentDateAndNumber
  DefineMessageFields_82; //Diadoc.Api.Proto.Docflow.BasicDocumentInfo
  DefineMessageFields_83; //Diadoc.Api.Proto.Docflow.InvoiceDocumentInfo
  DefineMessageFields_84; //Diadoc.Api.Proto.Docflow.InvoiceCorrectionDocumentInfo
  DefineMessageFields_85; //Diadoc.Api.Proto.Docflow.PriceListDocumentInfo
  DefineMessageFields_86; //Diadoc.Api.Proto.Docflow.ContractDocumentInfo
  DefineMessageFields_87; //Diadoc.Api.Proto.Docflow.SupplementaryAgreementDocumentInfo
  DefineMessageFields_88; //Diadoc.Api.Proto.Docflow.UniversalTransferDocumentInfo
  DefineMessageFields_89; //Diadoc.Api.Proto.Docflow.UniversalCorrectionDocumentInfo
  DefineMessageFields_90; //Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentSellerTitleInfo
  DefineMessageFields_91; //Diadoc.Api.Proto.Invoicing.Shipper
  DefineMessageFields_92; //Diadoc.Api.Proto.Invoicing.InvoiceTable
  DefineMessageFields_93; //Diadoc.Api.Proto.Invoicing.ExtendedInvoiceItem
  DefineMessageFields_94; //Diadoc.Api.Proto.Invoicing.TransferInfo
  DefineMessageFields_95; //Diadoc.Api.Proto.Invoicing.TransferBase
  DefineMessageFields_96; //Diadoc.Api.Proto.Invoicing.Waybill
  DefineMessageFields_97; //Diadoc.Api.Proto.Invoicing.Employee
  DefineMessageFields_98; //Diadoc.Api.Proto.Invoicing.OtherIssuer
  DefineMessageFields_99; //Diadoc.Api.Proto.Invoicing.AdditionalInfoId
  DefineMessageFields_100; //Diadoc.Api.Proto.Invoicing.UniversalTransferDocumentBuyerTitleInfo
  DefineMessageFields_101; //Diadoc.Api.Proto.Invoicing.UniversalCorrectionDocumentSellerTitleInfo
  DefineMessageFields_102; //Diadoc.Api.Proto.Invoicing.InvoiceForCorrectionInfo
  DefineMessageFields_103; //Diadoc.Api.Proto.Invoicing.InvoiceRevisionInfo
  DefineMessageFields_104; //Diadoc.Api.Proto.Invoicing.EventContent
  DefineMessageFields_105; //Diadoc.Api.Proto.Invoicing.CorrectionBase
  DefineMessageFields_106; //Diadoc.Api.Proto.Invoicing.InvoiceCorrectionTable
  DefineMessageFields_107; //Diadoc.Api.Proto.Invoicing.ExtendedInvoiceCorrectionItem
  DefineMessageFields_108; //Diadoc.Api.Proto.Invoicing.InvoiceInfo
  DefineMessageFields_109; //Diadoc.Api.Proto.Invoicing.AdditionalInfo
  DefineMessageFields_110; //Diadoc.Api.Proto.Invoicing.InvoiceItem
  DefineMessageFields_111; //Diadoc.Api.Proto.Invoicing.CustomsDeclaration
  DefineMessageFields_112; //Diadoc.Api.Proto.Invoicing.PaymentDocumentInfo
  DefineMessageFields_113; //Diadoc.Api.Proto.Invoicing.ShipperOrConsignee
  DefineMessageFields_114; //Diadoc.Api.Proto.Invoicing.InvoiceCorrectionInfo
  DefineMessageFields_115; //Diadoc.Api.Proto.Invoicing.InvoiceTotalsDiff
  DefineMessageFields_116; //Diadoc.Api.Proto.Invoicing.InvoiceCorrectionItem
  DefineMessageFields_117; //Diadoc.Api.Proto.Invoicing.CorrectableInvoiceItemFields
  DefineMessageFields_118; //Diadoc.Api.Proto.Invoicing.InvoiceItemAmountsDiff
  DefineMessageFields_119; //Diadoc.Api.Proto.Invoicing.Organizations.ExtendedOrganizationInfo
  DefineMessageFields_120; //Diadoc.Api.Proto.AsyncMethodResult
  DefineMessageFields_121; //Diadoc.Api.Proto.CertificateInfo
  DefineMessageFields_122; //Diadoc.Api.Proto.Certificates.CertificateInfoV2
  DefineMessageFields_123; //Diadoc.Api.Proto.Certificates.CertificateList
  DefineMessageFields_124; //Diadoc.Api.Proto.CloudSignRequest
  DefineMessageFields_125; //Diadoc.Api.Proto.CloudSignFile
  DefineMessageFields_126; //Diadoc.Api.Proto.CloudSignResult
  DefineMessageFields_127; //Diadoc.Api.Proto.CloudSignConfirmResult
  DefineMessageFields_128; //Diadoc.Api.Proto.AutosignReceiptsResult
  DefineMessageFields_129; //Diadoc.Api.Proto.Content_v2
  DefineMessageFields_130; //Diadoc.Api.Proto.Content
  DefineMessageFields_131; //Diadoc.Api.Proto.Content_v3
  DefineMessageFields_132; //Diadoc.Api.Proto.CounteragentList
  DefineMessageFields_133; //Diadoc.Api.Proto.Counteragent
  DefineMessageFields_134; //Diadoc.Api.Proto.CounteragentCertificateList
  DefineMessageFields_135; //Diadoc.Api.Proto.Certificate
  DefineMessageFields_136; //Diadoc.Api.Proto.OrganizationList
  DefineMessageFields_137; //Diadoc.Api.Proto.Organization
  DefineMessageFields_138; //Diadoc.Api.Proto.Department
  DefineMessageFields_139; //Diadoc.Api.Proto.Box
  DefineMessageFields_140; //Diadoc.Api.Proto.CustomPrintFormDetectionRequest
  DefineMessageFields_141; //Diadoc.Api.Proto.CustomPrintFormDetectionResult
  DefineMessageFields_142; //Diadoc.Api.Proto.CustomPrintFormDetectionItemResult
  DefineMessageFields_143; //Diadoc.Api.Proto.Departments.Department
  DefineMessageFields_144; //Diadoc.Api.Proto.Timestamp
  DefineMessageFields_145; //Diadoc.Api.Proto.Departments.Routing
  DefineMessageFields_146; //Diadoc.Api.Proto.Departments.DepartmentList
  DefineMessageFields_147; //Diadoc.Api.Proto.Departments.DepartmentToCreate
  DefineMessageFields_148; //Diadoc.Api.Proto.Departments.DepartmentToUpdate
  DefineMessageFields_149; //Diadoc.Api.Proto.Departments.ParentDepartmentPatch
  DefineMessageFields_150; //Diadoc.Api.Proto.Departments.DepartmentNamingPatch
  DefineMessageFields_151; //Diadoc.Api.Proto.Departments.DepartmentKppPatch
  DefineMessageFields_152; //Diadoc.Api.Proto.Departments.DepartmentAddressPatch
  DefineMessageFields_153; //Diadoc.Api.Proto.Departments.DepartmentRoutingPatch
  DefineMessageFields_154; //Diadoc.Api.Proto.Docflow.Entity
  DefineMessageFields_155; //Diadoc.Api.Proto.Docflow.Attachment
  DefineMessageFields_156; //Diadoc.Api.Proto.Docflow.Signature
  DefineMessageFields_157; //Diadoc.Api.Proto.Docflow.SignedAttachment
  DefineMessageFields_158; //Diadoc.Api.Proto.SignatureVerificationResult
  DefineMessageFields_159; //Diadoc.Api.Proto.CertificateVerificationResult
  DefineMessageFields_160; //Diadoc.Api.Proto.CertificateChainElement
  DefineMessageFields_161; //Diadoc.Api.Proto.Docflow.SignatureV3
  DefineMessageFields_162; //Diadoc.Api.Proto.Docflow.SignedAttachmentV3
  DefineMessageFields_163; //Diadoc.Api.Proto.Docflow.BilateralDocflow
  DefineMessageFields_164; //Diadoc.Api.Proto.Docflow.ReceiptDocflow
  DefineMessageFields_165; //Diadoc.Api.Proto.Docflow.RecipientSignatureDocflow
  DefineMessageFields_166; //Diadoc.Api.Proto.Docflow.RecipientSignatureRejectionDocflow
  DefineMessageFields_167; //Diadoc.Api.Proto.Docflow.Docflow
  DefineMessageFields_168; //Diadoc.Api.Proto.Docflow.DocflowStatus
  DefineMessageFields_169; //Diadoc.Api.Proto.Docflow.DocflowStatusModel
  DefineMessageFields_170; //Diadoc.Api.Proto.Docflow.InboundInvoiceDocflow
  DefineMessageFields_171; //Diadoc.Api.Proto.Docflow.OutboundInvoiceDocflow
  DefineMessageFields_172; //Diadoc.Api.Proto.Docflow.InvoiceConfirmationDocflow
  DefineMessageFields_173; //Diadoc.Api.Proto.Docflow.InboundInvoiceReceiptDocflow
  DefineMessageFields_174; //Diadoc.Api.Proto.Docflow.InvoiceCorrectionRequestDocflow
  DefineMessageFields_175; //Diadoc.Api.Proto.Docflow.UnilateralDocflow
  DefineMessageFields_176; //Diadoc.Api.Proto.Docflow.XmlBilateralDocflow
  DefineMessageFields_177; //Diadoc.Api.Proto.Docflow.BuyerTitleDocflow
  DefineMessageFields_178; //Diadoc.Api.Proto.Docflow.RevocationDocflow
  DefineMessageFields_179; //Diadoc.Api.Proto.Docflow.ResolutionDocflow
  DefineMessageFields_180; //Diadoc.Api.Proto.Docflow.InboundUniversalTransferDocumentDocflow
  DefineMessageFields_181; //Diadoc.Api.Proto.Docflow.OutboundUniversalTransferDocumentDocflow
  DefineMessageFields_182; //Diadoc.Api.Proto.Docflow.RoamingNotification
  DefineMessageFields_183; //Diadoc.Api.Proto.Docflow.GetDocflowBatchRequest
  DefineMessageFields_184; //Diadoc.Api.Proto.Docflow.GetDocflowRequest
  DefineMessageFields_185; //Diadoc.Api.Proto.Docflow.GetDocflowBatchResponse
  DefineMessageFields_186; //Diadoc.Api.Proto.Docflow.SearchDocflowsRequest
  DefineMessageFields_187; //Diadoc.Api.Proto.Docflow.SearchDocflowsResponse
  DefineMessageFields_188; //Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdRequest
  DefineMessageFields_189; //Diadoc.Api.Proto.Docflow.FetchedDocument
  DefineMessageFields_190; //Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponse
  DefineMessageFields_191; //Diadoc.Api.Proto.Docflow.GetDocflowEventsRequest
  DefineMessageFields_192; //Diadoc.Api.Proto.Docflow.GetDocflowEventsResponse
  DefineMessageFields_193; //Diadoc.Api.Proto.Docflow.DocflowEvent
  DefineMessageFields_194; //Diadoc.Api.Proto.TimeBasedFilter
  DefineMessageFields_195; //Diadoc.Api.Proto.Docflow.DocumentWithDocflow
  DefineMessageFields_196; //Diadoc.Api.Proto.ForwardDocumentEvent
  DefineMessageFields_197; //Diadoc.Api.Proto.Docflow.GetDocflowBatchResponseV3
  DefineMessageFields_198; //Diadoc.Api.Proto.Docflow.SearchDocflowsResponseV3
  DefineMessageFields_199; //Diadoc.Api.Proto.Docflow.FetchedDocumentV3
  DefineMessageFields_200; //Diadoc.Api.Proto.Docflow.GetDocflowsByPacketIdResponseV3
  DefineMessageFields_201; //Diadoc.Api.Proto.Docflow.GetDocflowEventsResponseV3
  DefineMessageFields_202; //Diadoc.Api.Proto.Docflow.DocflowEventV3
  DefineMessageFields_203; //Diadoc.Api.Proto.Docflow.DocumentWithDocflowV3
  DefineMessageFields_204; //Diadoc.Api.Proto.Docflow.LastEvent
  DefineMessageFields_205; //Diadoc.Api.Proto.Docflow.DocumentInfoV3
  DefineMessageFields_206; //Diadoc.Api.Proto.Docflow.DocumentParticipants
  DefineMessageFields_207; //Diadoc.Api.Proto.Docflow.DocumentParticipant
  DefineMessageFields_208; //Diadoc.Api.Proto.Docflow.DocumentLinks
  DefineMessageFields_209; //Diadoc.Api.Proto.Docflow.PacketInfo
  DefineMessageFields_210; //Diadoc.Api.Proto.Docflow.DocumentLetterInfo
  DefineMessageFields_211; //Diadoc.Api.Proto.Docflow.DocumentDraftInfo
  DefineMessageFields_212; //Diadoc.Api.Proto.Docflow.DocumentTemplateInfo
  DefineMessageFields_213; //Diadoc.Api.Proto.Docflow.TemplateTransformationInfo
  DefineMessageFields_214; //Diadoc.Api.Proto.Docflow.TemplateRefusalInfo
  DefineMessageFields_215; //Diadoc.Api.Proto.FullVersion
  DefineMessageFields_216; //Diadoc.Api.Proto.Documents.Document
  DefineMessageFields_217; //Diadoc.Api.Proto.Documents.LastOuterDocflow
  DefineMessageFields_218; //Diadoc.Api.Proto.Documents.ResolutionStatus
  DefineMessageFields_219; //Diadoc.Api.Proto.Documents.RecipientReceiptMetadata
  DefineMessageFields_220; //Diadoc.Api.Proto.Documents.SenderReceiptMetadata
  DefineMessageFields_221; //Diadoc.Api.Proto.Documents.ConfirmationMetadata
  DefineMessageFields_222; //Diadoc.Api.Proto.Documents.AmendmentRequestMetadata
  DefineMessageFields_223; //Diadoc.Api.Proto.Documents.Origin
  DefineMessageFields_224; //Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateMetadata
  DefineMessageFields_225; //Diadoc.Api.Proto.Documents.BilateralDocument.TrustConnectionRequestMetadata
  DefineMessageFields_226; //Diadoc.Api.Proto.Documents.BilateralDocument.BasicDocumentMetadata
  DefineMessageFields_227; //Diadoc.Api.Proto.Documents.BilateralDocument.PriceListMetadata
  DefineMessageFields_228; //Diadoc.Api.Proto.Documents.BilateralDocument.ContractMetadata
  DefineMessageFields_229; //Diadoc.Api.Proto.Documents.BilateralDocument.SupplementaryAgreementMetadata
  DefineMessageFields_230; //Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentMetadata
  DefineMessageFields_231; //Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceMetadata
  DefineMessageFields_232; //Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceRevisionMetadata
  DefineMessageFields_233; //Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionMetadata
  DefineMessageFields_234; //Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceCorrectionRevisionMetadata
  DefineMessageFields_235; //Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentMetadata
  DefineMessageFields_236; //Diadoc.Api.Proto.Documents.UnilateralDocument.ProformaInvoiceMetadata
  DefineMessageFields_237; //Diadoc.Api.Proto.Documents.UnilateralDocument.ServiceDetailsMetadata
  DefineMessageFields_238; //Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentMetadata
  DefineMessageFields_239; //Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentRevisionMetadata
  DefineMessageFields_240; //Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentMetadata
  DefineMessageFields_241; //Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalCorrectionDocumentRevisionMetadata
  DefineMessageFields_242; //Diadoc.Api.Proto.OuterDocflowInfo
  DefineMessageFields_243; //Diadoc.Api.Proto.Status
  DefineMessageFields_244; //Diadoc.Api.Proto.StatusDetail
  DefineMessageFields_245; //Diadoc.Api.Proto.Docflow.DocflowV3
  DefineMessageFields_246; //Diadoc.Api.Proto.Docflow.SenderTitleDocflow
  DefineMessageFields_247; //Diadoc.Api.Proto.Docflow.ConfirmationDocflow
  DefineMessageFields_248; //Diadoc.Api.Proto.Docflow.SignatureRejectionDocflow
  DefineMessageFields_249; //Diadoc.Api.Proto.Docflow.ParticipantResponseDocflow
  DefineMessageFields_250; //Diadoc.Api.Proto.Docflow.AmendmentRequestDocflow
  DefineMessageFields_251; //Diadoc.Api.Proto.Docflow.RevocationDocflowV3
  DefineMessageFields_252; //Diadoc.Api.Proto.Docflow.RevocationRequestDocflow
  DefineMessageFields_253; //Diadoc.Api.Proto.Docflow.RevocationResponseDocflow
  DefineMessageFields_254; //Diadoc.Api.Proto.Docflow.ReceiptDocflowV3
  DefineMessageFields_255; //Diadoc.Api.Proto.Docflow.OuterDocflow
  DefineMessageFields_256; //Diadoc.Api.Proto.Docflow.OuterDocflowEntities
  DefineMessageFields_257; //Diadoc.Api.Proto.Docflow.StatusEntity
  DefineMessageFields_258; //Diadoc.Api.Proto.Docflow.ResolutionDocflowV3
  DefineMessageFields_259; //Diadoc.Api.Proto.Docflow.ResolutionEntitiesV3
  DefineMessageFields_260; //Diadoc.Api.Proto.Docflow.ResolutionRequestV3
  DefineMessageFields_261; //Diadoc.Api.Proto.Docflow.ResolutionV3
  DefineMessageFields_262; //Diadoc.Api.Proto.Docflow.ApprovementSignatureV3
  DefineMessageFields_263; //Diadoc.Api.Proto.Docflow.SignatureDenialV3
  DefineMessageFields_264; //Diadoc.Api.Proto.Documents.DetectTitleResponse
  DefineMessageFields_265; //Diadoc.Api.Proto.Documents.DetectedDocumentTitle
  DefineMessageFields_266; //Diadoc.Api.Proto.Documents.DocumentList
  DefineMessageFields_267; //Diadoc.Api.Proto.Documents.DocumentProtocol
  DefineMessageFields_268; //Diadoc.Api.Proto.Documents.DocumentsMoveOperation
  DefineMessageFields_269; //Diadoc.Api.Proto.Documents.DocumentZipGenerationResult
  DefineMessageFields_270; //Diadoc.Api.Proto.Documents.Types.DetectedDocumentType
  DefineMessageFields_271; //Diadoc.Api.Proto.Documents.Types.DetectDocumentTypesResponse
  DefineMessageFields_272; //Diadoc.Api.Proto.Documents.Types.DocumentTypeDescriptionV2
  DefineMessageFields_273; //Diadoc.Api.Proto.Documents.Types.GetDocumentTypesResponseV2
  DefineMessageFields_274; //Diadoc.Api.Proto.Documents.Types.DocumentFunctionV2
  DefineMessageFields_275; //Diadoc.Api.Proto.Documents.Types.DocumentVersionV2
  DefineMessageFields_276; //Diadoc.Api.Proto.Documents.Types.DocumentWorkflowV2
  DefineMessageFields_277; //Diadoc.Api.Proto.Documents.Types.DocumentTitleV2
  DefineMessageFields_278; //Diadoc.Api.Proto.Documents.Types.SignerInfoV2
  DefineMessageFields_279; //Diadoc.Api.Proto.Documents.Types.DocumentMetadataItemV2
  DefineMessageFields_280; //Diadoc.Api.Proto.Dss.DssSignRequest
  DefineMessageFields_281; //Diadoc.Api.Proto.Dss.DssSignFile
  DefineMessageFields_282; //Diadoc.Api.Proto.Dss.DssSignResult
  DefineMessageFields_283; //Diadoc.Api.Proto.Dss.DssFileSigningResult
  DefineMessageFields_284; //Diadoc.Api.Proto.Employees.Employee
  DefineMessageFields_285; //Diadoc.Api.Proto.Employees.EmployeePermissions
  DefineMessageFields_286; //Diadoc.Api.Proto.Employees.EmployeeAction
  DefineMessageFields_287; //Diadoc.Api.Proto.Employees.EmployeeList
  DefineMessageFields_288; //Diadoc.Api.Proto.User
  DefineMessageFields_289; //Diadoc.Api.Proto.UserV2
  DefineMessageFields_290; //Diadoc.Api.Proto.FullName
  DefineMessageFields_291; //Diadoc.Api.Proto.OrganizationUserPermissions
  DefineMessageFields_292; //Diadoc.Api.Proto.AuthorizationPermission
  DefineMessageFields_293; //Diadoc.Api.Proto.Employees.EmployeeToCreate
  DefineMessageFields_294; //Diadoc.Api.Proto.Employees.EmployeeToCreateCredentials
  DefineMessageFields_295; //Diadoc.Api.Proto.Employees.EmployeeToCreateByLogin
  DefineMessageFields_296; //Diadoc.Api.Proto.Employees.EmployeeToCreateByCertificate
  DefineMessageFields_297; //Diadoc.Api.Proto.Employees.EmployeeToUpdate
  DefineMessageFields_298; //Diadoc.Api.Proto.Employees.EmployeePermissionsPatch
  DefineMessageFields_299; //Diadoc.Api.Proto.Employees.EmployeeDepartmentPatch
  DefineMessageFields_300; //Diadoc.Api.Proto.Employees.EmployeeIsAdministratorPatch
  DefineMessageFields_301; //Diadoc.Api.Proto.Employees.EmployeeDocumentAccessLevelPatch
  DefineMessageFields_302; //Diadoc.Api.Proto.Employees.EmployeeSelectedDepartmentsPatch
  DefineMessageFields_303; //Diadoc.Api.Proto.Employees.EmployeePositionPatch
  DefineMessageFields_304; //Diadoc.Api.Proto.Employees.EmployeeCanBeInvitedForChatPatch
  DefineMessageFields_305; //Diadoc.Api.Proto.Employees.AuthorizationPermissionPatch
  DefineMessageFields_306; //Diadoc.Api.Proto.Employees.Subscriptions.EmployeeSubscriptions
  DefineMessageFields_307; //Diadoc.Api.Proto.Employees.Subscriptions.SubscriptionsToUpdate
  DefineMessageFields_308; //Diadoc.Api.Proto.Employees.Subscriptions.Subscription
  DefineMessageFields_309; //Diadoc.Api.Proto.Events.CancellationInfo
  DefineMessageFields_310; //Diadoc.Api.Proto.Events.BoxEventList
  DefineMessageFields_311; //Diadoc.Api.Proto.Events.BoxEvent
  DefineMessageFields_312; //Diadoc.Api.Proto.Events.Message
  DefineMessageFields_313; //Diadoc.Api.Proto.Events.Template
  DefineMessageFields_314; //Diadoc.Api.Proto.Events.MessagePatch
  DefineMessageFields_315; //Diadoc.Api.Proto.Events.Entity
  DefineMessageFields_316; //Diadoc.Api.Proto.Events.EntityPatch
  DefineMessageFields_317; //Diadoc.Api.Proto.Events.TemplateToLetterTransformationInfo
  DefineMessageFields_318; //Diadoc.Api.Proto.Events.TemplateTransformationInfo
  DefineMessageFields_319; //Diadoc.Api.Proto.Events.TemplateRefusalInfo
  DefineMessageFields_320; //Diadoc.Api.Proto.Events.ResolutionRequestDenialInfo
  DefineMessageFields_321; //Diadoc.Api.Proto.Events.ResolutionRouteAssignmentInfo
  DefineMessageFields_322; //Diadoc.Api.Proto.Events.ResolutionRouteRemovalInfo
  DefineMessageFields_323; //Diadoc.Api.Proto.Events.RevocationRequestInfo
  DefineMessageFields_324; //Diadoc.Api.Proto.ExternalServiceAuthInfo
  DefineMessageFields_325; //Diadoc.Api.Proto.Forwarding.ForwardedDocumentId
  DefineMessageFields_326; //Diadoc.Api.Proto.Forwarding.ForwardedDocument
  DefineMessageFields_327; //Diadoc.Api.Proto.Forwarding.ForwardDocumentRequest
  DefineMessageFields_328; //Diadoc.Api.Proto.Forwarding.ForwardDocumentResponse
  DefineMessageFields_329; //Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsRequest
  DefineMessageFields_330; //Diadoc.Api.Proto.Forwarding.GetForwardedDocumentsResponse
  DefineMessageFields_331; //Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsRequest
  DefineMessageFields_332; //Diadoc.Api.Proto.Forwarding.GetForwardedDocumentEventsResponse
  DefineMessageFields_333; //Diadoc.Api.Proto.Forwarding.ForwardedDocumentEvent
  DefineMessageFields_334; //Diadoc.Api.Proto.GetOrganizationsByInnListRequest
  DefineMessageFields_335; //Diadoc.Api.Proto.OrganizationWithCounteragentStatus
  DefineMessageFields_336; //Diadoc.Api.Proto.GetOrganizationsByInnListResponse
  DefineMessageFields_337; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552SellerTitleInfo
  DefineMessageFields_338; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552TransferInfo
  DefineMessageFields_339; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkDescription
  DefineMessageFields_340; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552WorkItem
  DefineMessageFields_341; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificate552BuyerTitleInfo
  DefineMessageFields_342; //Diadoc.Api.Proto.Invoicing.TovTorgSellerTitleInfo
  DefineMessageFields_343; //Diadoc.Api.Proto.Invoicing.TovTorgBuyerTitleInfo
  DefineMessageFields_344; //Diadoc.Api.Proto.Invoicing.TovTorgTable
  DefineMessageFields_345; //Diadoc.Api.Proto.Invoicing.TovTorgItem
  DefineMessageFields_346; //Diadoc.Api.Proto.Invoicing.TovTorgTransferInfo
  DefineMessageFields_347; //Diadoc.Api.Proto.Invoicing.GroundInfo
  DefineMessageFields_348; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSellerTitleInfo
  DefineMessageFields_349; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificateBuyerTitleInfo
  DefineMessageFields_350; //Diadoc.Api.Proto.Invoicing.AcceptanceCertificateSignatureInfo
  DefineMessageFields_351; //Diadoc.Api.Proto.Invoicing.WorkDescription
  DefineMessageFields_352; //Diadoc.Api.Proto.Invoicing.WorkItem
  DefineMessageFields_353; //Diadoc.Api.Proto.Invoicing.Official
  DefineMessageFields_354; //Diadoc.Api.Proto.Invoicing.Attorney
  DefineMessageFields_355; //Diadoc.Api.Proto.Invoicing.FnsRegistrationMessageInfo
  DefineMessageFields_356; //Diadoc.Api.Proto.Invoicing.InvoiceCorrectionRequestInfo
  DefineMessageFields_357; //Diadoc.Api.Proto.Invoicing.RevocationRequestInfo
  DefineMessageFields_358; //Diadoc.Api.Proto.Invoicing.SignatureRejectionInfo
  DefineMessageFields_359; //Diadoc.Api.Proto.Invoicing.Torg12SellerTitleInfo
  DefineMessageFields_360; //Diadoc.Api.Proto.Invoicing.Torg12BuyerTitleInfo
  DefineMessageFields_361; //Diadoc.Api.Proto.Invoicing.Torg12Item
  DefineMessageFields_362; //Diadoc.Api.Proto.Invoicing.Grounds
  DefineMessageFields_363; //Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageEntry
  DefineMessageFields_364; //Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetRequest
  DefineMessageFields_365; //Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiGetResponse
  DefineMessageFields_366; //Diadoc.Api.Proto.KeyValueStorage.KeyValueStorageApiPutRequest
  DefineMessageFields_367; //Diadoc.Api.Proto.LoginPassword
  DefineMessageFields_368; //Diadoc.Api.Proto.StringValue
  DefineMessageFields_369; //Diadoc.Api.Proto.OrganizationPropertiesToUpdate
  DefineMessageFields_370; //Diadoc.Api.Proto.HeadOrganizationPropertiesToUpdate
  DefineMessageFields_371; //Diadoc.Api.Proto.Organizations.AutoBlockStatus
  DefineMessageFields_372; //Diadoc.Api.Proto.Organizations.BlockStatus
  DefineMessageFields_373; //Diadoc.Api.Proto.Organizations.ManualBlockStatus
  DefineMessageFields_374; //Diadoc.Api.Proto.Organizations.OrganizationFeatures
  DefineMessageFields_375; //Diadoc.Api.Proto.OrganizationUser
  DefineMessageFields_376; //Diadoc.Api.Proto.OrganizationUsersList
  DefineMessageFields_377; //Diadoc.Api.Proto.Recognition.Recognized
  DefineMessageFields_378; //Diadoc.Api.Proto.Recognition.RecognizedInvoice
  DefineMessageFields_379; //Diadoc.Api.Proto.Registration.RegistrationRequest
  DefineMessageFields_380; //Diadoc.Api.Proto.Registration.RegistrationResponse
  DefineMessageFields_381; //Diadoc.Api.Proto.Registration.RegistrationConfirmRequest
  DefineMessageFields_382; //Diadoc.Api.Proto.ResolutionRouteList
  DefineMessageFields_383; //Diadoc.Api.Proto.ResolutionRoute
  DefineMessageFields_384; //Diadoc.Api.Proto.RoamingNotification
  DefineMessageFields_385; //Diadoc.Api.Proto.SignatureInfo
  DefineMessageFields_386; //Diadoc.Api.Proto.Users.UserToUpdate
  DefineMessageFields_387; //Diadoc.Api.Proto.Users.UserLoginPatch
  DefineMessageFields_388; //Diadoc.Api.Proto.Users.UserFullNamePatch

  DefineEnumItems_1; //Diadoc.Api.Proto.Events.CustomDataPatchOperation
  DefineEnumItems_2; //Diadoc.Api.Proto.LockMode
  DefineEnumItems_3; //Diadoc.Api.Proto.ResolutionRequestType
  DefineEnumItems_4; //Diadoc.Api.Proto.ResolutionType
  DefineEnumItems_5; //Diadoc.Api.Proto.Invoicing.Signers.SignerType
  DefineEnumItems_6; //Diadoc.Api.Proto.Invoicing.Signers.SignerPowers
  DefineEnumItems_7; //Diadoc.Api.Proto.Invoicing.Signers.SignerStatus
  DefineEnumItems_8; //Diadoc.Api.Proto.Invoicing.Signers.DocumentTitleType
  DefineEnumItems_9; //Diadoc.Api.Proto.ResolutionAction
  DefineEnumItems_10; //Diadoc.Api.Proto.DocumentType
  DefineEnumItems_11; //Diadoc.Api.Proto.DocumentDirection
  DefineEnumItems_12; //Diadoc.Api.Proto.Invoicing.FunctionType
  DefineEnumItems_13; //Diadoc.Api.Proto.Invoicing.ItemMark
  DefineEnumItems_14; //Diadoc.Api.Proto.Invoicing.InvoiceFormatVersion
  DefineEnumItems_15; //Diadoc.Api.Proto.Invoicing.TaxRate
  DefineEnumItems_16; //Diadoc.Api.Proto.Invoicing.Organizations.OrgType
  DefineEnumItems_17; //Diadoc.Api.Proto.Certificates.CertificateType
  DefineEnumItems_18; //Diadoc.Api.Proto.CounteragentStatus
  DefineEnumItems_19; //Diadoc.Api.Proto.OrganizationInvoiceFormatVersion
  DefineEnumItems_20; //Diadoc.Api.Proto.Sociability
  DefineEnumItems_21; //Diadoc.Api.Proto.Docflow.DocflowStatusSeverity
  DefineEnumItems_22; //Diadoc.Api.Proto.Docflow.SearchScope
  DefineEnumItems_23; //Diadoc.Api.Proto.TotalCountType
  DefineEnumItems_24; //Diadoc.Api.Proto.SortDirection
  DefineEnumItems_25; //Diadoc.Api.Proto.Documents.ResolutionStatusType
  DefineEnumItems_26; //Diadoc.Api.Proto.Documents.RevocationStatus
  DefineEnumItems_27; //Diadoc.Api.Proto.Documents.RoamingNotificationStatus
  DefineEnumItems_28; //Diadoc.Api.Proto.Documents.SenderSignatureStatus
  DefineEnumItems_29; //Diadoc.Api.Proto.Documents.ProxySignatureStatus
  DefineEnumItems_30; //Diadoc.Api.Proto.Documents.GeneralReceiptStatus
  DefineEnumItems_31; //Diadoc.Api.Proto.Documents.RecipientResponseStatus
  DefineEnumItems_32; //Diadoc.Api.Proto.Documents.MessageType
  DefineEnumItems_33; //Diadoc.Api.Proto.Documents.AcceptanceCertificateDocument.AcceptanceCertificateDocumentStatus
  DefineEnumItems_34; //Diadoc.Api.Proto.Documents.ReceiptStatus
  DefineEnumItems_35; //Diadoc.Api.Proto.Documents.BilateralDocument.BilateralDocumentStatus
  DefineEnumItems_36; //Diadoc.Api.Proto.Documents.InvoiceDocument.InvoiceStatus
  DefineEnumItems_37; //Diadoc.Api.Proto.Documents.NonformalizedDocument.NonformalizedDocumentStatus
  DefineEnumItems_38; //Diadoc.Api.Proto.Documents.UnilateralDocument.UnilateralDocumentStatus
  DefineEnumItems_39; //Diadoc.Api.Proto.Documents.UniversalTransferDocument.UniversalTransferDocumentStatus
  DefineEnumItems_40; //Diadoc.Api.Proto.OuterStatusType
  DefineEnumItems_41; //Diadoc.Api.Proto.Docflow.ResolutionStatus
  DefineEnumItems_42; //Diadoc.Api.Proto.Dss.DssConfirmType
  DefineEnumItems_43; //Diadoc.Api.Proto.Dss.DssOperator
  DefineEnumItems_44; //Diadoc.Api.Proto.Dss.DssFileSigningStatus
  DefineEnumItems_45; //Diadoc.Api.Proto.Dss.DssOperationStatus
  DefineEnumItems_46; //Diadoc.Api.Proto.DocumentAccessLevel
  DefineEnumItems_47; //Diadoc.Api.Proto.Events.TemplateRefusalType
  DefineEnumItems_48; //Diadoc.Api.Proto.Events.EntityType
  DefineEnumItems_49; //Diadoc.Api.Proto.Events.AttachmentType
  DefineEnumItems_50; //Diadoc.Api.Proto.Recognition.RecognizedDocumentType
  DefineEnumItems_51; //Diadoc.Api.Proto.Registration.RegistrationStatus

end;

procedure FreeTypes;
var
  ProtoType: TsanPBCustomType;
begin
  for ProtoType in ProtoTypeList do ProtoType.Free;
  ProtoTypeList.Free;
end;

initialization
  CreateTypes;

finalization
  FreeTypes;

end.
