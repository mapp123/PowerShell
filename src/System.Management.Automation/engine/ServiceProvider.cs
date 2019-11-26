using System;
using System.Collections.Concurrent;
using System.Configuration.Internal;
using System.Text;

namespace System.Management.Automation
{
    internal class ServiceProvider
    {
        internal static readonly ServiceProvider Singleton = new ServiceProvider();

        private readonly ConcurrentDictionary<Type, object> _services;

        private ServiceProvider()
        {
            _services = new ConcurrentDictionary<Type, object>();
        }

        internal ServiceProvider AddSingleton<IComponent, TImplementation>()
            where IComponent : class
            where TImplementation : IComponent, new()
        {
            _services.AddOrUpdate(typeof(IComponent), t => new TImplementation(), (t, o) => new TImplementation());
            return this;
        }

        internal ServiceProvider AddSingleton<IComponent, TImplementation>(TImplementation service)
            where IComponent : class
            where TImplementation : IComponent
        {
            _services.AddOrUpdate(typeof(IComponent), service, (t, o) => service);
            return this;
        }

        internal IComponent GetService<IComponent>() where IComponent : class
        {
            if (_services.TryGetValue(typeof(IComponent), out object value))
            {
                return (IComponent)value;
            }

            return null;
        }
    }
}
