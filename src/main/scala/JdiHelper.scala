import com.sun.jdi.connect.{AttachingConnector, Connector, LaunchingConnector}
import com.sun.jdi.{Bootstrap, ReferenceType, VirtualMachine}

import java.time.Instant
import scala.collection.mutable
import scala.concurrent.duration._
import scala.io.StdIn
import scala.jdk.CollectionConverters._


//VM Options:
//  -Xdebug
//  -Xrunjdwp:transport=dt_socket,address=4000,server=y,suspend=y
object JdiHelperApp extends App {

  val vm = JdiHelper.connect(hostname = "127.0.0.1", port = 4000)

//  vm.resume()

  while (true) {
    Thread.sleep(1.seconds.toMillis)

    val immutableCollectionClasses: mutable.Buffer[ReferenceType] = vm.allClasses().asScala //.filter(_.name().startsWith("scala"))
    val classCounts = immutableCollectionClasses.map(_.name()) zip vm.instanceCounts(immutableCollectionClasses.asJava)
    val topClassCounts = classCounts.sortBy { case (_, count) => count }
      .reverse
      .take(10)

    println( topClassCounts mkString ("\n"))
    println(s"======================================== ${Instant.now().toString} ========================================")
  }
}

object JdiHelper {

  def connect(hostname: String, port: Int, timeout: Int = 1000): VirtualMachine = {
    val attachingConnectors = Bootstrap.virtualMachineManager().attachingConnectors().asScala

    val socketAttach: AttachingConnector =
      attachingConnectors
        .find(_.name() == "com.sun.jdi.SocketAttach")
        .getOrElse(throw new IllegalStateException)

    val socketAttachArguments = socketAttach.defaultArguments()

    val hostnameArg = socketAttachArguments.get("hostname").asInstanceOf[Connector.StringArgument]
    val portArg = socketAttachArguments.get("port").asInstanceOf[Connector.IntegerArgument]
    val timeoutArg = socketAttachArguments.get("timeout").asInstanceOf[Connector.IntegerArgument]

    hostnameArg.setValue(hostname)
    portArg.setValue(port)
    timeoutArg.setValue(timeout)

    socketAttach.attach(Map(
      "hostname" -> hostnameArg.asInstanceOf[Connector.Argument],
      "port" -> portArg.asInstanceOf[Connector.Argument],
      "timeout" -> timeoutArg.asInstanceOf[Connector.Argument]
    ).asJava)
  }
}