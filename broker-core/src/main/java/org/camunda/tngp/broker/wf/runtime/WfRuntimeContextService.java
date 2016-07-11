package org.camunda.tngp.broker.wf.runtime;

import org.camunda.tngp.broker.services.HashIndexManager;
import org.camunda.tngp.broker.wf.repository.WfTypeCacheService;
import org.camunda.tngp.broker.wf.runtime.bpmn.handler.BpmnEventHandler;
import org.camunda.tngp.broker.wf.runtime.bpmn.handler.CreateActivityInstanceHandler;
import org.camunda.tngp.broker.wf.runtime.bpmn.handler.StartProcessHandler;
import org.camunda.tngp.broker.wf.runtime.bpmn.handler.TakeInitialFlowsHandler;
import org.camunda.tngp.broker.wf.runtime.bpmn.handler.TaskEventHandler;
import org.camunda.tngp.broker.wf.runtime.bpmn.handler.WaitEventHandler;
import org.camunda.tngp.broker.wf.runtime.idx.ActivityInstanceIndexWriter;
import org.camunda.tngp.hashindex.Long2LongHashIndex;
import org.camunda.tngp.log.Log;
import org.camunda.tngp.log.LogReader;
import org.camunda.tngp.log.LogWriter;
import org.camunda.tngp.log.idgenerator.IdGenerator;
import org.camunda.tngp.servicecontainer.Injector;
import org.camunda.tngp.servicecontainer.Service;
import org.camunda.tngp.servicecontainer.ServiceContext;

public class WfRuntimeContextService implements Service<WfRuntimeContext>
{
    // TODO: move somewhere else?
    protected static final int READ_BUFFER_SIZE = 1024 * 1024;

    protected final Injector<WfTypeCacheService> wfTypeChacheInjector = new Injector<>();
    protected final Injector<IdGenerator> idGeneratorInjector = new Injector<>();
    protected final Injector<Log> logInjector = new Injector<>();

    protected final Injector<HashIndexManager<Long2LongHashIndex>> activityInstanceIndexInjector = new Injector<>();

    protected final WfRuntimeContext wfRuntimeContext;

    public WfRuntimeContextService(int id, String name)
    {
        wfRuntimeContext = new WfRuntimeContext(id, name);
    }

    @Override
    public void start(ServiceContext serviceContext)
    {
        wfRuntimeContext.setWfTypeCacheService(wfTypeChacheInjector.getValue());
        wfRuntimeContext.setIdGenerator(idGeneratorInjector.getValue());

        final Log log = logInjector.getValue();
        final LogWriter logWriter = new LogWriter(log);

        wfRuntimeContext.setLogWriter(logWriter);

        final LogReader logReader = new LogReader(log, READ_BUFFER_SIZE);
        final BpmnEventHandler bpmnEventHandler = new BpmnEventHandler(wfTypeChacheInjector.getValue(), logReader, logWriter, idGeneratorInjector.getValue());
        bpmnEventHandler.addFlowElementHandler(new StartProcessHandler());
        bpmnEventHandler.addFlowElementHandler(new CreateActivityInstanceHandler());

        bpmnEventHandler.addProcessHandler(new TakeInitialFlowsHandler());

        final WaitEventHandler waitEventHandler = new WaitEventHandler();
        bpmnEventHandler.addProcessHandler(waitEventHandler);
        bpmnEventHandler.addFlowElementHandler(waitEventHandler);

        wfRuntimeContext.setBpmnEventHandler(bpmnEventHandler);

        final TaskEventHandler taskEventHandler = new TaskEventHandler(
                new LogReader(log, READ_BUFFER_SIZE),
                logWriter,
                activityInstanceIndexInjector.getValue().getIndex());
        wfRuntimeContext.setTaskEventHandler(taskEventHandler);

        wfRuntimeContext.setActivityInstanceIndexWriter(
                new ActivityInstanceIndexWriter(
                    new LogReader(log, READ_BUFFER_SIZE),
                    activityInstanceIndexInjector.getValue().getIndex()));
    }

    @Override
    public void stop()
    {
        // nothing to do
    }

    @Override
    public WfRuntimeContext get()
    {
        return wfRuntimeContext;
    }

    public Injector<WfTypeCacheService> getWfTypeChacheInjector()
    {
        return wfTypeChacheInjector;
    }

    public Injector<IdGenerator> getIdGeneratorInjector()
    {
        return idGeneratorInjector;
    }

    public Injector<Log> getLogInjector()
    {
        return logInjector;
    }

    public Injector<HashIndexManager<Long2LongHashIndex>> getActivityInstanceIndexInjector()
    {
        return activityInstanceIndexInjector;
    }

}
